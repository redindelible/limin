use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use slotmap::SecondaryMap;
use crate::{hir, llvm};
use crate::hir::{FunctionKey, MayBreak, NameKey, StructKey};
use crate::llvm::{Builder, CallingConvention, Constant, GEPIndex, GlobalRef, ParameterRef, Value};

pub fn generate_llvm(hir: hir::HIR) -> llvm::Module {
    let mut gen = Codegen::new(&hir);
    gen.generate();
    gen.llvm
}

struct StructInfo {
    llvm_ref: llvm::StructRef,
    fields: HashMap<String, usize>,
    trace_fn: llvm::FunctionRef,
    type_info: GlobalRef
}

struct FunctionInfo {
    llvm_ref: llvm::FunctionRef,
    params: HashMap<String, llvm::ValueRef>,
    frame_param: ParameterRef,
    closure_param: ParameterRef
}

struct NameInfo {
    llvm_ref: llvm::ValueRef,
}

struct Codegen<'a> {
    llvm: llvm::Module,
    hir: &'a hir::HIR<'a>,

    structs: SecondaryMap<StructKey, StructInfo>,
    functions: SecondaryMap<FunctionKey, FunctionInfo>,
    names: SecondaryMap<NameKey, NameInfo>,
    mangle: RefCell<u32>,

    create_object_fn: llvm::ValueRef,
    mark_object_fn: llvm::ValueRef,
    trace_stack_fn: llvm::ValueRef,
}

struct FrameInfo<'a> {
    ty: llvm::TypeRef,
    indices: HashMap<NameKey, usize>,
    parent: Option<&'a FrameInfo<'a>>,
    llvm_ref: llvm::ValueRef,
    stack_index: u32,
    stack: RefCell<u32>
}

impl FrameInfo<'_> {
    fn push(&self, value: &llvm::ValueRef, builder: &Builder) {
        let curr_top = *self.stack.borrow();
        let top = builder.gep(None, Rc::clone(&self.ty), Rc::clone(&self.llvm_ref), vec![
            GEPIndex::ConstantIndex(0),
            GEPIndex::ConstantIndex(self.stack_index),
            GEPIndex::ConstantIndex(curr_top)
        ]).to_value();
        builder.store(&top, value);
        *self.stack.borrow_mut() += 1;
        let stack_size_ptr = builder.gep(None, Rc::clone(&self.ty), Rc::clone(&self.llvm_ref), vec![
            GEPIndex::ConstantIndex(0),
            GEPIndex::ConstantIndex(2),
        ]).to_value();
        builder.store(&stack_size_ptr, &llvm::Types::int_constant(64, *self.stack.borrow() as u64).to_value());
    }

    fn clear(&self, builder: &Builder) {
        let stack_size_ptr = builder.gep(None, Rc::clone(&self.ty), Rc::clone(&self.llvm_ref), vec![
            GEPIndex::ConstantIndex(0),
            GEPIndex::ConstantIndex(2),
        ]).to_value();
        builder.store(&stack_size_ptr, &llvm::Types::int_constant(64, 0).to_value());
        *self.stack.borrow_mut() = 0;
    }

    fn pop_state(&self) -> u32 {
        *self.stack.borrow()
    }

    fn pop(&self, state: u32, builder: &Builder) {
        let stack_size_ptr = builder.gep(None, Rc::clone(&self.ty), Rc::clone(&self.llvm_ref), vec![
            GEPIndex::ConstantIndex(0),
            GEPIndex::ConstantIndex(2),
        ]).to_value();
        *self.stack.borrow_mut() = state;
        builder.store(&stack_size_ptr, &llvm::Types::int_constant(64, state as u64).to_value());
    }
}

enum ParentFrame<'a> {
    Caller(llvm::ValueRef),
    Block(&'a FrameInfo<'a>)
}

struct StackSim<'a> {
    hir: &'a hir::HIR<'a>,
    curr: usize,
    max: usize
}

impl StackSim<'_> {
    fn sim(hir: &hir::HIR, expr: &hir::Expr) -> usize {
        let mut sim = StackSim::new(hir);
        sim._sim_expr(expr);
        sim.max
    }

    fn sim_stmts(hir: &hir::HIR, stmts: &Vec<hir::Stmt>) -> usize {
        let mut sim = StackSim::new(hir);
        sim._sim_stmts(stmts);
        sim.max
    }

    fn new<'a>(hir: &'a hir::HIR<'a>) -> StackSim<'a> {
        StackSim {
            hir, curr: 0, max: 0
        }
    }

    fn _sim_stmts(&mut self, stmts: &Vec<hir::Stmt>) {
        for stmt in stmts {
            match stmt {
                hir::Stmt::Expr { expr, .. } => {
                    self._sim_expr(expr);
                    self.curr = 0;
                }
                hir::Stmt::Decl { value, .. } => {
                    self._sim_expr(value);
                    self.curr = 0;
                }
                hir::Stmt::Return { value, .. } => {
                    self._sim_expr(value);
                    self.curr = 0;
                }
            }
        }
    }

    fn _sim_expr(&mut self, expr: &hir::Expr) {
        match expr {
            hir::Expr::Integer { .. } => {
                self.push_n(0);
            }
            hir::Expr::Name { decl, .. } => {
                let slots = self.stack_slots(&self.hir.type_of_name(*decl));
                self.push_n(slots);
            }
            hir::Expr::Block { .. } => {
                let slots = self.stack_slots(&self.hir.type_of_expr(expr));
                self.push_n(slots);
            }
            hir::Expr::Call { callee, arguments, .. } => {
                let reset_to = self.curr;
                self._sim_expr(callee);
                for arg in arguments {
                    self._sim_expr(arg);
                }
                self.curr = reset_to;
                let slots = self.stack_slots(&self.hir.type_of_expr(expr));
                self.push_n(slots);
            }
            hir::Expr::New { fields, .. } => {
                let reset_to = self.curr;
                for field in fields.values() {
                    self._sim_expr(field);
                }
                self.curr = reset_to;
                self.push_n(1);
            }
            _ => {
                panic!("{:?}", expr);
                todo!()
            }
        }
    }

    fn push_n(&mut self, slots: usize) {
        self.curr += slots;
        if self.curr > self.max {
            self.max = self.curr;
        }
    }

    fn stack_slots(&self, ty: &hir::Type) -> usize {
        match ty {
            hir::Type::Integer { .. } => 0,
            hir::Type::Function { .. } => 1,
            hir::Type::Struct { .. } => 1,
            hir::Type::Boolean => 0,
            hir::Type::Unit => 0,
            _ => {
                panic!("{:?}", ty);
                todo!()
            }
        }
    }
}


impl Codegen<'_> {
    fn new<'a>(hir: &'a hir::HIR<'a>) -> Codegen<'a> {
        let mut llvm = llvm::Module::new(hir.name.clone());

        let create_object_fn = llvm.add_function(
            "limin_create_object",
            llvm::Types::ptr(),
            vec![
                llvm::Parameter::new("type".into(), llvm::Types::ptr()),
                llvm::Parameter::new("frame".into(), llvm::Types::ptr())
            ],
            CallingConvention::CCC
        ).to_value();

        let mark_object_fn = llvm.add_function(
            "limin_mark_object",
            llvm::Types::void(),
            vec![llvm::Parameter::new("obj".into(), llvm::Types::ptr())],
            CallingConvention::CCC
        ).to_value();

        let trace_stack_fn = llvm.add_function(
            "limin_trace_stack",
            llvm::Types::void(),
            vec![
                llvm::Parameter::new("stack".into(), llvm::Types::ptr()),
                llvm::Parameter::new("count".into(), llvm::Types::int(64))
            ],
            CallingConvention::CCC
        ).to_value();

        Codegen {
            llvm,
            hir,
            structs: SecondaryMap::new(),
            functions: SecondaryMap::new(),
            names: SecondaryMap::new(),
            mangle: RefCell::new(0),
            create_object_fn,
            mark_object_fn,
            trace_stack_fn
        }
    }

    fn gen_mangled2(&self, prefix: &'static str, name: &str) -> String {
        let num = *self.mangle.borrow();
        *self.mangle.borrow_mut() += 1;
        format!("{prefix}_{name}_{:>08X}", num)
    }

    fn gen_mangled(&self, prefix: &'static str) -> String {
        let num = *self.mangle.borrow();
        *self.mangle.borrow_mut() += 1;
        format!("{prefix}_{:>08X}", num)
    }

    fn generate(&mut self) {
        let hir = self.hir;
        for (key, _) in &hir.structs {
            let name = self.gen_mangled2("struct", &hir.structs[key].name);
            let struct_ref = self.llvm.types.add_struct(name);

            let obj_ptr = llvm::Parameter::new("obj".into(), llvm::Types::ptr());
            let trace_fn = self.llvm.add_function(
                &self.gen_mangled2("trace", &hir.structs[key].name),
                llvm::Types::void(),
                vec![
                    Rc::clone(&obj_ptr)
                ],
                CallingConvention::CCC
            );

            let type_info = self.llvm.add_global_constant(self.gen_mangled2("typeinfo", &hir.structs[key].name), llvm::Types::struct_constant(vec![
                llvm::Types::function_constant(&trace_fn),
                llvm::Types::sizeof(&struct_ref.as_type_ref())
            ]));

            let info = StructInfo { llvm_ref: struct_ref, fields: HashMap::new(), trace_fn, type_info };
            self.structs.insert(key, info);
        }

        for (key, struct_) in &hir.structs {
            let struct_ty = self.structs[key].llvm_ref.as_type_ref();
            let mut fields = vec![
                llvm::Types::ptr(),  // type info ptr
                llvm::Types::ptr(),  // next obj ptr
                llvm::Types::int(8),  // the gc mark field
            ];
            let trace_builder = Builder::new(&self.structs[key].trace_fn);
            let obj_ptr = Rc::clone(&self.structs[key].trace_fn.parameters[0]);

            for field in struct_.fields.values() {
                let index = fields.len();
                self.structs[key].fields.insert(field.name.clone(), fields.len());

                let field_ptr = trace_builder.gep(None, Rc::clone(&struct_ty), Rc::clone(&obj_ptr).to_value(), vec![
                    GEPIndex::ConstantIndex(0),
                    GEPIndex::ConstantIndex(index as u32)
                ]);
                self.emit_tracing_code(&field.typ, field_ptr.to_value(), &trace_builder);

                fields.push(self.generate_type(&field.typ));
            }
            trace_builder.ret_void();
            self.structs[key].llvm_ref.set_fields(fields);
        }

        for (key, proto ) in &hir.function_prototypes {
            let name = self.gen_mangled2("function", &proto.name);

            let ret = self.generate_type(&proto.ret);
            let frame_param = Rc::new(llvm::Parameter { name: "frame".into(), typ: llvm::Types::ptr() });
            let closure_param = Rc::new(llvm::Parameter { name: "closure".into(), typ: llvm::Types::ptr() });
            let mut params: Vec<ParameterRef> = vec![
                Rc::clone(&frame_param),
                Rc::clone(&closure_param)
            ];
            let mut llvm_params: HashMap<String, llvm::ValueRef> = HashMap::new();
            for param in &proto.params {
                let name = self.gen_mangled2("param", &param.name);
                let llvm_param = Rc::new(llvm::Parameter { name: name.clone(), typ: self.generate_type(&param.typ) });
                llvm_params.insert(param.name.clone(), Rc::clone(&llvm_param).to_value());
                params.push(llvm_param);
            }

            let func_ref = self.llvm.add_function(&name, ret, params, CallingConvention::FastCC);

            self.functions.insert(key, FunctionInfo { llvm_ref: Rc::clone(&func_ref), params: llvm_params, frame_param, closure_param });

            let func_global = self.llvm.add_global_constant(self.gen_mangled2("funcglobal", &proto.name), llvm::Types::struct_constant(vec![
                llvm::Types::function_constant(&func_ref),
                llvm::Types::null()
            ]));

            self.names.insert(proto.decl, NameInfo { llvm_ref: func_global.to_value() });
        }

        for (key, body) in &hir.function_bodies {
            let proto = &hir.function_prototypes[key];
            let builder = Builder::new(&self.functions[key].llvm_ref);

            let max_stack = StackSim::sim(&self.hir, &body.body);

            let frame_info = self.create_frame(&body.declared, max_stack, &builder, ParentFrame::Caller(Rc::clone(&self.functions[key].frame_param).to_value()));

            for param in &proto.params {
                let param_ptr = Self::get_name_ptr(&param.decl, &builder, &frame_info);
                builder.store(&param_ptr, &self.functions[key].params[&param.name]);
            }

            let res = self.generate_expr(&body.body, &builder, &frame_info);
            if !body.body.does_break() {
                builder.ret(res);
            }
        }

        let main_key = hir.main_function.unwrap();
        let main_info = &self.functions[main_key];
        let main = self.llvm.add_function("main", llvm::Types::int(32), vec![], CallingConvention::CCC);
        let builder = Builder::new(&main);
        let exit_code = builder.call(None, CallingConvention::FastCC, llvm::Types::int(32), &Rc::clone(&main_info.llvm_ref).to_value(), vec![
            llvm::Types::null().to_value(),
            llvm::Types::null().to_value()
        ]);
        builder.ret(exit_code.to_value());
    }

    fn create_frame<'a>(&mut self, declared: &Vec<NameKey>, max_stack: usize, builder: &Builder, parent: ParentFrame<'a>) -> FrameInfo<'a> {
        let mut items = Vec::new();
        let mut map = HashMap::new();
        items.push(llvm::Types::ptr());   // the parent frame pointer
        items.push(llvm::Types::ptr());   // the trace fn
        items.push(llvm::Types::int(64));   // the stack size

        let frame_ptr = llvm::Parameter::new("frame".into(), llvm::Types::ptr());
        let trace_fn = self.llvm.add_function(&self.gen_mangled("frametrace"), llvm::Types::void(), vec![
            Rc::clone(&frame_ptr)
        ], CallingConvention::CCC);

        for decl in declared {
            let index = items.len();
            map.insert(*decl, index);
            let ty = self.generate_type(&self.hir.type_of_name(*decl));
            items.push(ty);
        }
        let stack_index = items.len() as u32;
        items.push(llvm::Types::array(&llvm::Types::ptr(), max_stack));

        let frame_ty = llvm::Types::struct_(items);

        let trace_builder = Builder::new(&trace_fn);
        for decl in declared {
            let item_ptr = trace_builder.gep(None, Rc::clone(&frame_ty), Rc::clone(&frame_ptr).to_value(), vec![
                GEPIndex::ConstantIndex(0),
                GEPIndex::ConstantIndex(map[decl] as u32)
            ]).to_value();
            self.emit_tracing_code(&self.hir.type_of_name(*decl), item_ptr, &trace_builder);
        }

        let stack_ptr = trace_builder.gep(None, Rc::clone(&frame_ty), Rc::clone(&frame_ptr).to_value(), vec![
            GEPIndex::ConstantIndex(0),
            GEPIndex::ConstantIndex(stack_index)
        ]).to_value();
        let count_ptr = trace_builder.gep(None, Rc::clone(&frame_ty), Rc::clone(&frame_ptr).to_value(), vec![
            GEPIndex::ConstantIndex(0),
            GEPIndex::ConstantIndex(2)
        ]).to_value();
        let count = trace_builder.load(None, llvm::Types::int(64), count_ptr).to_value();
        trace_builder.call_void(CallingConvention::CCC, &self.trace_stack_fn, vec![
            stack_ptr, count
        ]);

        trace_builder.ret_void();

        let llvm_ref = builder.alloca(Some(self.gen_mangled("frame")), Rc::clone(&frame_ty)).to_value();
        let parent_ptr = builder.gep(None, Rc::clone(&frame_ty), Rc::clone(&llvm_ref), vec![
            GEPIndex::ConstantIndex(0),
            GEPIndex::ConstantIndex(0)
        ]).to_value();
        let parent = match parent {
            ParentFrame::Block(parent_frame) => {
                builder.store(&parent_ptr, &Rc::clone(&parent_frame.llvm_ref));
                Some(parent_frame)
            }
            ParentFrame::Caller(value) => {
                builder.store(&parent_ptr, &value);
                None
            }
        };
        let trace_fn_ptr = builder.gep(None, Rc::clone(&frame_ty), Rc::clone(&llvm_ref), vec![
            GEPIndex::ConstantIndex(0),
            GEPIndex::ConstantIndex(1)
        ]);
        builder.store(&trace_fn_ptr.to_value(), &llvm::Types::function_constant(&trace_fn).to_value());

        let stack_ptr = builder.gep(None, Rc::clone(&frame_ty), Rc::clone(&llvm_ref), vec![
            GEPIndex::ConstantIndex(0),
            GEPIndex::ConstantIndex(2)
        ]);
        builder.store(&stack_ptr.to_value(), &llvm::Types::int_constant(64, 0).to_value());
        FrameInfo { ty: frame_ty, indices: map, parent, llvm_ref, stack_index, stack: RefCell::new(0) }
    }

    fn get_name_ptr(name: &NameKey, builder: &Builder, frame: &FrameInfo) -> llvm::ValueRef {
        let mut curr = frame;
        loop {
            match curr.indices.get(name) {
                Some(index) => {
                    return builder.gep(None, Rc::clone(&curr.ty), Rc::clone(&curr.llvm_ref), vec![
                        GEPIndex::ArrayIndex(llvm::Types::int_constant(64, 0).to_value()),
                        GEPIndex::ConstantIndex(*index as u32)
                    ]).to_value();
                }
                None => {
                    curr = curr.parent.unwrap();
                }
            }
        }
    }

    fn generate_expr(&mut self, expr: &hir::Expr, builder: &Builder, frame: &FrameInfo) -> llvm::ValueRef {
        match expr {
            hir::Expr::Integer { num, .. } => {
                llvm::Types::int_constant(32, *num).to_value()
            }
            hir::Expr::Name { decl, .. } => {
                let ptr;
                if self.names.contains_key(*decl) {
                    ptr = Rc::clone(&self.names[*decl].llvm_ref)
                } else {
                    ptr = Self::get_name_ptr(decl, builder, frame);
                }
                let ty = self.generate_type(&self.hir.type_of_name(*decl));
                let value = builder.load(None, ty, ptr).to_value();
                self.push_to_stack(&self.hir.type_of_name(*decl), Rc::clone(&value), builder, frame);
                value
            }
            hir::Expr::Block { stmts, trailing_expr, declared, .. } => {
                let max_stack = StackSim::sim_stmts(self.hir, stmts);
                let block_frame = self.create_frame(declared, max_stack, builder, ParentFrame::Block(frame));
                for stmt in stmts {
                    self.generate_stmt(stmt, builder, &block_frame);
                }
                let value = match trailing_expr.as_ref() {
                    Some(expr) => {
                        self.generate_expr(expr, builder, &block_frame)
                    },
                    None => {
                        llvm::Types::int_constant(32, 0).to_value()
                    }
                };
                self.push_to_stack(&self.hir.type_of_expr(expr), Rc::clone(&value), builder, frame);
                value
            }
            hir::Expr::Call { callee, arguments, .. } => {
                let hir::Type::Function { ret, .. } = self.hir.type_of_expr(callee) else { panic!() };

                let stack_state = frame.pop_state();

                let callee = self.generate_expr(callee, builder, frame);

                let func = builder.extractvalue(None, &callee, vec![0]);
                let closure = builder.extractvalue(None, &callee, vec![1]);

                let mut args = vec![
                    Rc::clone(&frame.llvm_ref),
                    closure.to_value()
                ];
                args.extend(arguments.iter().map(|arg| self.generate_expr(arg, builder, frame)));

                let value = builder.call(None, CallingConvention::FastCC, self.generate_type(&ret), &func.to_value(), args).to_value();

                frame.pop(stack_state, builder);
                self.push_to_stack(&self.hir.type_of_expr(expr), Rc::clone(&value), builder, frame);
                value
            }
            hir::Expr::New { struct_, fields, .. } => {
                let stack_state = frame.pop_state();

                let type_info = &self.structs[*struct_].type_info;
                let value = builder.call(None, CallingConvention::CCC, llvm::Types::ptr(), &self.create_object_fn, vec![
                    Rc::clone(type_info).to_value(),
                    Rc::clone(&frame.llvm_ref)
                ]).to_value();

                for (field_name, field_init) in fields {
                    let index = self.structs[*struct_].fields[field_name];
                    let init = self.generate_expr(field_init, builder, frame);
                    let field_ptr = builder.gep(None, self.structs[*struct_].llvm_ref.as_type_ref(), Rc::clone(&value), vec![
                        GEPIndex::ConstantIndex(0),
                        GEPIndex::ConstantIndex(index as u32)
                    ]).to_value();
                    builder.store(&field_ptr, &init);
                    self.push_to_stack(&self.hir.type_of_expr(field_init), init, builder, frame);
                }

                frame.pop(stack_state, builder);
                self.push_to_stack(&self.hir.type_of_expr(expr), Rc::clone(&value), builder, frame);
                value
            }
            _ => panic!("{:?}", expr)
        }
    }

    fn push_to_stack(&self, ty: &hir::Type, value: llvm::ValueRef, builder: &Builder, frame: &FrameInfo) {
        match ty {
            hir::Type::Integer { .. } => { }
            hir::Type::Boolean => { }
            hir::Type::Unit => { }
            hir::Type::Function { .. } => {
                let obj = builder.extractvalue(None, &value, vec![1]);
                frame.push(&obj.to_value(), builder);
            }
            hir::Type::Struct { .. } => {
                frame.push(&value, builder)
            },
            _ => {
                panic!("{:?}", ty);
                todo!()
            }
        }
    }

    fn generate_stmt(&mut self, stmt: &hir::Stmt, builder: &Builder, frame: &FrameInfo) {
        match stmt {
            hir::Stmt::Return { value, .. } => {
                let value = self.generate_expr(value, builder, frame);
                builder.ret(value);
            }
            hir::Stmt::Expr { expr, .. } => {
                self.generate_expr(expr, builder, frame);
                frame.clear(builder);
            }
            hir::Stmt::Decl { decl, value, .. } => {
                let expr = self.generate_expr(value, builder, frame);
                let ptr = Self::get_name_ptr(decl, builder, frame);
                builder.store(&ptr, &expr);
                frame.clear(builder);
            }
        }
    }

    fn generate_type(&self, ty: &hir::Type) -> llvm::TypeRef {
        match ty {
            hir::Type::Integer { bits } => llvm::Types::int(*bits),
            hir::Type::Boolean => llvm::Types::int(1),
            hir::Type::Function { .. } => llvm::Types::struct_(vec![llvm::Types::ptr(), llvm::Types::ptr()]),
            hir::Type::Struct { .. } => llvm::Types::ptr(),
            _ => panic!("{:?}", ty)
        }
    }

    fn emit_tracing_code(&self, ty: &hir::Type, value: llvm::ValueRef, builder: &Builder) {
        match ty {
            hir::Type::Integer { .. } => { },
            hir::Type::Boolean { .. } => { },
            hir::Type::Function { .. } => {
                let closure_ty = self.generate_type(ty);
                let closure_ptr = builder.gep(None, closure_ty, value, vec![
                    GEPIndex::ConstantIndex(0), GEPIndex::ConstantIndex(1)
                ]).to_value();
                builder.call_void(CallingConvention::CCC, &self.mark_object_fn, vec![
                    closure_ptr
                ]);
            },
            hir::Type::Struct { .. } => {
                builder.call_void(CallingConvention::CCC, &self.mark_object_fn, vec![
                    value
                ]);
            },
            _ => panic!("{:?}", ty)
        }
    }
}

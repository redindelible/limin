use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use indexmap::IndexMap;
use slotmap::SecondaryMap;
use crate::{lir, llvm};
use crate::common::map_join;
use crate::lir::{BlockKey, LocalKey};
// use crate::hir::{FunctionKey, MayBreak, NameKey, StructKey};
use crate::llvm::{Builder, CallingConvention, Constant, GEPIndex, GlobalRef, ParameterRef, Value};

pub fn generate_llvm(lir: lir::LIR) -> llvm::Module {
    let mut gen = Codegen::new(&lir);
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
    value_ref: llvm::ValueRef,
    params: IndexMap<String, llvm::ValueRef>,
    frame_param: ParameterRef,
    closure_param: ParameterRef
}

struct Codegen<'a> {
    llvm: llvm::Module,
    lir: &'a lir::LIR,

    structs: SecondaryMap<lir::StructKey, StructInfo>,
    functions: SecondaryMap<lir::FunctionKey, FunctionInfo>,

    mangle: RefCell<u32>,

    create_object_fn: llvm::ValueRef,
    mark_object_fn: llvm::ValueRef,
    trace_stack_fn: llvm::ValueRef,
}

struct FrameInfo<'a> {
    ty: llvm::TypeRef,
    indices: HashMap<LocalKey, usize>,
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

struct StackSim {
    curr: usize,
    max: usize
}

impl StackSim {
    fn sim(lir: &lir::LIR, expr: &lir::Expr) -> usize {
        let mut sim = StackSim { curr: 0, max: 0 };
        sim._sim_expr(expr, lir);
        sim.max
    }

    fn sim_stmts(lir: &lir::LIR, stmts: &Vec<lir::Stmt>) -> usize {
        let mut sim = StackSim { curr: 0, max: 0 };
        sim._sim_stmts(stmts, lir);
        sim.max
    }

    fn _sim_stmts(&mut self, stmts: &Vec<lir::Stmt>, lir: &lir::LIR) {
        for stmt in stmts {
            match stmt {
                lir::Stmt::Expr(expr) => {
                    self._sim_expr(expr, lir);
                    self.curr = 0;
                }
                lir::Stmt::Decl(_, value) => {
                    self._sim_expr(value, lir);
                    self.curr = 0;
                }
                lir::Stmt::Ret(value) => {
                    self._sim_expr(value, lir);
                    self.curr = 0;
                }
            }
        }
    }

    fn _sim_expr(&mut self, expr: &lir::Expr, lir: &lir::LIR) {
        match expr {
            lir::Expr::Integer(_) | lir::Expr::Unit | lir::Expr::Boolean(_) => {
                self.push_n(0);
            },
            lir::Expr::Never => panic!("I don't think this is possible"),
            lir::Expr::LoadLocal(local) => {
                let slots = self.stack_slots(&lir.locals[*local].typ);
                self.push_n(slots);
            }
            lir::Expr::StoreLocal(_, expr) => {
                self._sim_expr(expr, lir);
            }
            lir::Expr::GetAttr(_, expr, _) => {
                let reset_to = self.curr;
                self._sim_expr(expr, lir);
                self.curr = reset_to;
                let slots = self.stack_slots(&lir.type_of(expr));
                self.push_n(slots);
            }
            lir::Expr::LoadFunction(_) => {
                self.push_n(1);
            }
            lir::Expr::Parameter(func, index) => {
                let (_, param) = &lir.function_prototypes[*func].params[*index];
                self.push_n(self.stack_slots(param));
            }
            lir::Expr::Block(block) => {
                let slots = self.stack_slots(&lir.blocks[*block].ret_type);
                self.push_n(slots);
            }
            lir::Expr::Call(callee, arguments) => {
                let reset_to = self.curr;
                self._sim_expr(callee, lir);
                for arg in arguments {
                    self._sim_expr(arg, lir);
                }
                self.curr = reset_to;
                let slots = self.stack_slots(&lir.type_of(expr));
                self.push_n(slots);
            }
            lir::Expr::New(_, fields) => {
                let reset_to = self.curr;
                for expr in fields.values() {
                    self._sim_expr(expr, lir);
                }
                self.curr = reset_to;
                self.push_n(1);
            }
            // _ => {
            //     panic!("{:?}", expr);
            // }
        }
    }

    fn push_n(&mut self, slots: usize) {
        self.curr += slots;
        if self.curr > self.max {
            self.max = self.curr;
        }
    }

    fn stack_slots(&self, ty: &lir::Type) -> usize {
        match ty {
            lir::Type::Never => 0,
            lir::Type::Unit => 0,
            lir::Type::Integer(_) => 0,
            lir::Type::Boolean => 0,
            lir::Type::Function(_, _) => 1,
            lir::Type::Struct { .. } => 1,
            // _ => {
            //     panic!("{:?}", ty);
            //     todo!()
            // }
        }
    }
}


impl Codegen<'_> {
    fn new(lir: &lir::LIR) -> Codegen {
        let mut llvm = llvm::Module::new("placeholder".to_string());  // todo

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
            lir,
            structs: SecondaryMap::new(),
            functions: SecondaryMap::new(),
            mangle: RefCell::new(0),
            create_object_fn,
            mark_object_fn,
            trace_stack_fn,
        }
    }

    fn mangled<const N: usize>(&self, things: [&str; N]) -> String {
        let num = *self.mangle.borrow();
        *self.mangle.borrow_mut() += 1;
        format!("\"{}_{:>08X}\"", things.join("_"), num)
    }

    fn generate(&mut self) {
        let lir = self.lir;
        for (key, struct_proto) in &lir.struct_prototypes {
            let name = self.mangled(["struct", &struct_proto.name]);
            let struct_ref = self.llvm.types.add_struct(name);

            let obj_ptr = llvm::Parameter::new("obj".into(), llvm::Types::ptr());
            let trace_fn = self.llvm.add_function(
                &self.mangled(["trace", &struct_proto.name]),
                llvm::Types::void(),
                vec![obj_ptr.clone()],
                CallingConvention::CCC
            );

            let type_info = self.llvm.add_global_constant(self.mangled(["typeinfo", &struct_proto.name]), llvm::Types::struct_constant(vec![
                llvm::Types::function_constant(&trace_fn),
                llvm::Types::sizeof(&struct_ref.as_type_ref())
            ]));

            let info = StructInfo { llvm_ref: struct_ref, fields: HashMap::new(), trace_fn, type_info };
            self.structs.insert(key, info);
        }

        for (key, struct_body) in &lir.struct_bodies {
            let struct_ty = self.structs[key].llvm_ref.as_type_ref();
            let mut fields = vec![
                llvm::Types::ptr(),  // type info ptr
                llvm::Types::ptr(),  // next obj ptr
                llvm::Types::int(8),  // the gc mark field
            ];
            let trace_builder = Builder::new(&self.structs[key].trace_fn);
            let obj_ptr = Rc::clone(&self.structs[key].trace_fn.parameters[0]);

            for (field_name, field_type) in &struct_body.fields {
                let index = fields.len();
                self.structs[key].fields.insert(field_name.clone(), fields.len());

                let field_ptr = trace_builder.gep(None, Rc::clone(&struct_ty), Rc::clone(&obj_ptr).to_value(), vec![
                    GEPIndex::ConstantIndex(0),
                    GEPIndex::ConstantIndex(index as u32)
                ]);
                self.emit_tracing_code(&field_type, field_ptr.to_value(), &trace_builder);

                fields.push(self.generate_type(&field_type));
            }
            trace_builder.ret_void();
            self.structs[key].llvm_ref.set_fields(fields);
        }

        for (key, proto ) in &lir.function_prototypes {
            let name = self.mangled(["function", &proto.name]);

            let ret = self.generate_type(&proto.ret);
            let frame_param = Rc::new(llvm::Parameter { name: "frame".into(), typ: llvm::Types::ptr() });
            let closure_param = Rc::new(llvm::Parameter { name: "closure".into(), typ: llvm::Types::ptr() });
            let mut params: Vec<ParameterRef> = vec![
                Rc::clone(&frame_param),
                Rc::clone(&closure_param)
            ];
            let mut llvm_params: IndexMap<String, llvm::ValueRef> = IndexMap::new();
            for (param_name, param_type) in &proto.params {
                let name = self.mangled(["param", param_name]);
                let llvm_param = Rc::new(llvm::Parameter { name: name.clone(), typ: self.generate_type(param_type) });
                llvm_params.insert(param_name.clone(), llvm_param.clone().to_value());
                params.push(llvm_param);
            }

            let func_ref = self.llvm.add_function(&name, ret, params, CallingConvention::FastCC);

            let func_global = self.llvm.add_global_constant(self.mangled(["funcglobal", &proto.name]), llvm::Types::struct_constant(vec![
                llvm::Types::function_constant(&func_ref),
                llvm::Types::null()
            ]));

            let info = FunctionInfo {
                llvm_ref: Rc::clone(&func_ref), params: llvm_params, frame_param, closure_param,
                value_ref: func_global.to_value(),
            };

            self.functions.insert(key, info);
        }

        for (key, body) in &lir.function_bodies {
            let proto = &lir.function_prototypes[key];

            let builder = Builder::new(&self.functions[key].llvm_ref);

            let parent_frame = self.functions[key].frame_param.clone().to_value();
            let frame_info = self.create_frame(body.body, &builder, ParentFrame::Caller(parent_frame));

            let res = self.generate_block(&body.body, &builder, &frame_info);
            if let Some(result) = res {
                builder.ret(result);
            }
        }

        let main_key = lir.main_fn;
        let main_info = &self.functions[main_key];
        let main = self.llvm.add_function("main", llvm::Types::int(32), vec![], CallingConvention::CCC);
        let builder = Builder::new(&main);
        let exit_code = builder.call(None, CallingConvention::FastCC, llvm::Types::int(32), &Rc::clone(&main_info.llvm_ref).to_value(), vec![
            llvm::Types::null().to_value(),
            llvm::Types::null().to_value()
        ]);
        builder.ret(exit_code.to_value());
    }

    fn create_frame<'a>(&mut self, block: BlockKey, builder: &Builder, parent: ParentFrame<'a>) -> FrameInfo<'a> {
        let mut items = Vec::new();
        let mut map = HashMap::new();
        items.push(llvm::Types::ptr());   // the parent frame pointer
        items.push(llvm::Types::ptr());   // the trace fn
        items.push(llvm::Types::int(64));   // the stack size

        let frame_ptr = llvm::Parameter::new("frame".into(), llvm::Types::ptr());
        let trace_fn = self.llvm.add_function(
            &self.mangled(["frametrace"]),
            llvm::Types::void(),
            vec![frame_ptr.clone()],
            CallingConvention::CCC
        );

        for local in &self.lir.blocks[block].locals {
            let index = items.len();
            map.insert(*local, index);
            let ty = self.generate_type(&self.lir.locals[*local].typ);
            items.push(ty);
        }
        let stack_index = items.len() as u32;
        let max_stack = StackSim::sim_stmts(&self.lir, &self.lir.blocks[block].stmts);
        items.push(llvm::Types::array(&llvm::Types::ptr(), max_stack));

        let frame_ty = llvm::Types::struct_(items);

        let trace_builder = Builder::new(&trace_fn);
        for local in &self.lir.blocks[block].locals {
            let item_ptr = trace_builder.gep(None, Rc::clone(&frame_ty), Rc::clone(&frame_ptr).to_value(), vec![
                GEPIndex::ConstantIndex(0),
                GEPIndex::ConstantIndex(map[&local] as u32)
            ]).to_value();
            self.emit_tracing_code(&self.lir.locals[*local].typ, item_ptr, &trace_builder);
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

        let llvm_ref = builder.alloca(Some(self.mangled(["frame"])), Rc::clone(&frame_ty)).to_value();
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

    fn get_name_ptr(local: LocalKey, builder: &Builder, frame: &FrameInfo) -> llvm::ValueRef {
        let mut curr = frame;
        loop {
            match curr.indices.get(&local) {
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

    fn generate_block(&mut self, block: &BlockKey, builder: &Builder, frame: &FrameInfo) -> Option<llvm::ValueRef> {
        let lir::Block { stmts, ret, locals, ret_type } = &self.lir.blocks[*block];
        let block_frame = self.create_frame(*block, builder, ParentFrame::Block(frame));
        for stmt in stmts {
            self.generate_stmt(stmt, builder, &block_frame);
        }
        if matches!(ret.as_ref(), lir::Expr::Never) {
            None
        } else {
            let value = self.generate_expr(&ret, builder, &block_frame);
            Some(value)
        }
    }

    fn generate_expr(&mut self, expr: &lir::Expr, builder: &Builder, frame: &FrameInfo) -> llvm::ValueRef {
        match expr {
            lir::Expr::Never => panic!(),
            lir::Expr::Unit => llvm::Types::int_constant(1, 0).to_value(),
            lir::Expr::Integer(num) => llvm::Types::int_constant(32, *num).to_value(),
            lir::Expr::Boolean(value) => llvm::Types::int_constant(1, *value as u64).to_value(),
            lir::Expr::LoadLocal(local) => {
                let ptr = Self::get_name_ptr(*local, builder, frame);
                let ty = &self.lir.locals[*local].typ;
                let value = builder.load(None, self.generate_type(ty), ptr).to_value();
                self.push_to_stack(ty, Rc::clone(&value), builder, frame);
                value
            }
            lir::Expr::StoreLocal(_, _) => todo!(),
            lir::Expr::GetAttr(struct_, expr, attr) => {
                let stack_state = frame.pop_state();

                let obj = self.generate_expr(expr, builder, frame);
                let struct_info = &self.structs[*struct_];
                let index = struct_info.fields[attr];

                let field_ty = self.generate_type(&self.lir.struct_bodies[*struct_].fields[attr]);

                let field_ptr = builder.gep(None, struct_info.llvm_ref.as_type_ref(), obj, vec![
                    GEPIndex::ConstantIndex(0),
                    GEPIndex::ConstantIndex(index as u32),
                ]).to_value();
                let field = builder.load(None, field_ty.clone(), field_ptr).to_value();
                frame.pop(stack_state, builder);
                self.push_to_stack(&self.lir.struct_bodies[*struct_].fields[attr], field.clone(), builder, frame);
                field
            },
            lir::Expr::LoadFunction(func) => {
                let value = builder.load(None, Self::function_type(), self.functions[*func].value_ref.clone()).to_value();
                self.push_to_stack(&self.lir.function_prototypes[*func].sig(), value.clone(), builder, frame);
                value
            },
            lir::Expr::Parameter(func, index) => {
                let (_, param) = self.functions[*func].params.get_index(*index).unwrap();
                self.push_to_stack(&self.lir.function_prototypes[*func].params[*index].1, param.clone(), builder, frame);
                param.clone()
            },
            lir::Expr::Block(block) => {
                let value = self.generate_block(block, builder, frame).unwrap();
                self.push_to_stack(&self.lir.blocks[*block].ret_type, value.clone(), builder, frame);
                value
            },
            lir::Expr::Call(callee, arguments) => {
                let lir::Type::Function(_, ret) = self.lir.type_of(callee) else { panic!() };

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
                self.push_to_stack(&ret, Rc::clone(&value), builder, frame);
                value
            },
            lir::Expr::New(struct_, fields) => {
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
                    self.push_to_stack(&self.lir.type_of(field_init), init, builder, frame);
                }

                frame.pop(stack_state, builder);
                self.push_to_stack(&self.lir.type_of(expr), Rc::clone(&value), builder, frame);
                value
            }
            // _ => panic!("{:?}", expr)
        }
    }

    fn push_to_stack(&self, ty: &lir::Type, value: llvm::ValueRef, builder: &Builder, frame: &FrameInfo) {
        match ty {
            lir::Type::Never => panic!(),
            lir::Type::Unit => { }
            lir::Type::Boolean => { }
            lir::Type::Integer(_) => { }
            lir::Type::Function(_, _) => {
                let obj = builder.extractvalue(None, &value, vec![1]);
                frame.push(&obj.to_value(), builder);
            }
            lir::Type::Struct(_) => {
                frame.push(&value, builder)
            }
        }
    }

    fn generate_stmt(&mut self, stmt: &lir::Stmt, builder: &Builder, frame: &FrameInfo) {
        match stmt {
            lir::Stmt::Ret(value) => {
                let value = self.generate_expr(value, builder, frame);
                builder.ret(value);
            }
            lir::Stmt::Expr(expr) => {
                self.generate_expr(expr, builder, frame);
                frame.clear(builder);
            }
            lir::Stmt::Decl(decl, value) => {
                let expr = self.generate_expr(value, builder, frame);
                let ptr = Self::get_name_ptr(*decl, builder, frame);
                builder.store(&ptr, &expr);
                frame.clear(builder);
            }
        }
    }

    fn generate_type(&self, ty: &lir::Type) -> llvm::TypeRef {
        match ty {
            lir::Type::Never => panic!(),
            lir::Type::Unit => todo!(),
            lir::Type::Boolean => llvm::Types::int(1),
            lir::Type::Integer(bits) => llvm::Types::int(*bits),
            lir::Type::Function(_, _) => Self::function_type(),
            lir::Type::Struct(_) => llvm::Types::ptr(),
        }
    }

    fn function_type() -> llvm::TypeRef {
        llvm::Types::struct_(vec![llvm::Types::ptr(), llvm::Types::ptr()])
    }

    fn emit_tracing_code(&self, ty: &lir::Type, value: llvm::ValueRef, builder: &Builder) {
        match ty {
            lir::Type::Unit => { },
            lir::Type::Never => panic!(),
            lir::Type::Integer(_) => { },
            lir::Type::Boolean => { },
            lir::Type::Function(_, _) => {
                let closure_ty = self.generate_type(ty);
                let closure_ptr = builder.gep(None, closure_ty, value, vec![
                    GEPIndex::ConstantIndex(0), GEPIndex::ConstantIndex(1)
                ]).to_value();
                builder.call_void(CallingConvention::CCC, &self.mark_object_fn, vec![
                    closure_ptr
                ]);
            },
            lir::Type::Struct(_) => {
                builder.call_void(CallingConvention::CCC, &self.mark_object_fn, vec![
                    value
                ]);
            },
        }
    }
}

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use slotmap::SecondaryMap;
use crate::{hir, llvm};
use crate::hir::{FunctionKey, MayBreak, NameKey, StructKey};
use crate::llvm::{Builder, CallingConvention, Constant, FunctionRef, GEPIndex, GlobalRef, ParameterRef, Value};

pub fn generate_llvm(hir: hir::HIR) -> llvm::Module {
    let mut gen = Codegen::new(&hir);
    gen.generate();
    gen.llvm
}

struct StructInfo {
    llvm_ref: llvm::StructRef,
    fields: HashMap<String, usize>,
    trace_fn: FunctionRef,
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
}

struct FrameInfo<'a> {
    ty: llvm::TypeRef,
    indices: HashMap<NameKey, usize>,
    parent: Option<&'a FrameInfo<'a>>,
    llvm_ref: llvm::ValueRef
}

enum ParentFrame<'a> {
    Caller(llvm::ValueRef),
    Block(&'a FrameInfo<'a>)
}

impl Codegen<'_> {
    fn new<'a>(hir: &'a hir::HIR<'a>) -> Codegen<'a> {
        let mut llvm = llvm::Module::new(hir.name.clone());

        let create_object_fn = llvm.add_function(
            "limin_create_object",
            llvm::Types::ptr(),
            vec![llvm::Parameter::new("type".into(), llvm::Types::ptr())],
            CallingConvention::CCC
        ).to_value();

        let mark_object_fn = llvm.add_function(
            "limin_mark_object",
            llvm::Types::void(),
            vec![llvm::Parameter::new("obj".into(), llvm::Types::ptr())],
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
            mark_object_fn
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
            // let trace_builder = Builder::new(&trace_fn);
            // for (i, field) in hir.structs[key].fields.values().enumerate() {
            //     let field_ptr = trace_builder.gep(None, struct_ref.as_type_ref(), obj_ptr.to_value(), vec![
            //         GEPIndex::ConstantIndex(0),
            //         GEPIndex::ConstantIndex(i)
            //     ]
            //
            //     )
            //
            //     self.emit_tracing_code(&field.typ)
            // }

            let mut fields = Vec::new();
            for field in struct_.fields.values() {
                self.structs[key].fields.insert(field.name.clone(), fields.len());
                fields.push(self.generate_type(&field.typ));
            }
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

            let frame_info = self.create_frame(&body.declared, &builder, ParentFrame::Caller(Rc::clone(&self.functions[key].frame_param).to_value()));

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

    fn create_frame<'a>(&mut self, declared: &Vec<NameKey>, builder: &Builder, parent: ParentFrame<'a>) -> FrameInfo<'a> {
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
        let frame_ty = llvm::Types::struct_(items);

        let trace_builder = Builder::new(&trace_fn);
        for decl in declared {
            let item_ptr = trace_builder.gep(None, Rc::clone(&frame_ty), Rc::clone(&frame_ptr).to_value(), vec![
                GEPIndex::ConstantIndex(0),
                GEPIndex::ConstantIndex(map[decl] as u32)
            ]).to_value();
            self.emit_tracing_code(&self.hir.type_of_name(*decl), item_ptr, &trace_builder);
        }
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
        FrameInfo { ty: frame_ty, indices: map, parent, llvm_ref }
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
                builder.load(None, ty, ptr).to_value()
            }
            hir::Expr::Block { stmts, trailing_expr, declared, .. } => {
                let frame = self.create_frame(declared, builder, ParentFrame::Block(frame));
                for stmt in stmts {
                    self.generate_stmt(stmt, builder, &frame);
                }
                match trailing_expr.as_ref() {
                    Some(expr) => {
                        self.generate_expr(expr, builder, &frame)
                    },
                    None => {
                        llvm::Types::int_constant(32, 0).to_value()
                    }
                }
            }
            hir::Expr::Call { callee, arguments, .. } => {
                let hir::Type::Function { ret, .. } = self.hir.type_of_expr(callee) else { panic!() };
                let callee = self.generate_expr(callee, builder, frame);

                let func = builder.extractvalue(None, &callee, vec![0]);
                let closure = builder.extractvalue(None, &callee, vec![1]);

                let mut args = vec![
                    Rc::clone(&frame.llvm_ref),
                    closure.to_value()
                ];
                args.extend(arguments.iter().map(|arg| self.generate_expr(arg, builder, frame)));

                builder.call(None, CallingConvention::FastCC, self.generate_type(&ret), &func.to_value(), args).to_value()
            }
            hir::Expr::New { struct_, fields, .. } => {
                let type_info = &self.structs[*struct_].type_info;
                let obj = builder.call(None, CallingConvention::CCC, llvm::Types::ptr(), &self.create_object_fn, vec![
                    Rc::clone(type_info).to_value()
                ]);

                obj.to_value()
            }
            _ => panic!("{:?}", expr)
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
            }
            hir::Stmt::Decl { decl, value, .. } => {
                let expr = self.generate_expr(value, builder, frame);
                let ptr = Self::get_name_ptr(decl, builder, frame);
                builder.store(&ptr, &expr);
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

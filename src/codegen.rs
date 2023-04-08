use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use slotmap::SecondaryMap;
use crate::{hir, llvm};
use crate::hir::{FunctionKey, MayBreak, NameKey, StructKey};
use crate::llvm::{Builder, CallingConvention, GEPIndex, Value};

pub fn generate_llvm(hir: hir::HIR) -> llvm::Module {
    let mut gen = Codegen::new(&hir);
    gen.generate();
    gen.llvm
}

struct StructInfo {
    llvm_ref: llvm::StructRef,
    fields: HashMap<String, usize>
}

struct FunctionInfo {
    llvm_ref: llvm::FunctionRef,
    params: HashMap<String, llvm::ValueRef>
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
}

struct FrameInfo<'a> {
    ty: llvm::TypeRef,
    indices: HashMap<NameKey, usize>,
    parent: Option<&'a FrameInfo<'a>>,
    llvm_ref: llvm::ValueRef
}

impl Codegen<'_> {
    fn new<'a>(hir: &'a hir::HIR<'a>) -> Codegen<'a> {
        Codegen {
            llvm: llvm::Module::new(hir.name.clone()),
            hir,
            structs: SecondaryMap::new(),
            functions: SecondaryMap::new(),
            names: SecondaryMap::new(),
            mangle: RefCell::new(0)
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
            let info = StructInfo { llvm_ref: struct_ref, fields: HashMap::new() };
            self.structs.insert(key, info);
        }

        for (key, struct_) in &hir.structs {
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
            let mut params: Vec<llvm::ParameterRef> = vec![
                Rc::new(llvm::Parameter { name: "frame".into(), typ: llvm::Types::ptr() }),
                Rc::new(llvm::Parameter { name: "closure".into(), typ: llvm::Types::ptr() }),
            ];
            let mut llvm_params: HashMap<String, llvm::ValueRef> = HashMap::new();
            for param in &proto.params {
                let name = self.gen_mangled2("param", &param.name);
                let llvm_param = Rc::new(llvm::Parameter { name: name.clone(), typ: self.generate_type(&param.typ) });
                llvm_params.insert(param.name.clone(), Rc::clone(&llvm_param).to_value());
                params.push(llvm_param);
            }

            let func_ref = self.llvm.add_function(&name, ret, params, CallingConvention::FastCC);

            self.functions.insert(key, FunctionInfo { llvm_ref: Rc::clone(&func_ref), params: llvm_params });

            let func_global = self.llvm.add_global_constant(self.gen_mangled2("funcglobal", &proto.name), llvm::Types::struct_constant(vec![
                llvm::Types::function_constant(&func_ref),
                llvm::Types::null()
            ]));

            self.names.insert(proto.decl, NameInfo { llvm_ref: func_global.to_value() });
        }

        for (key, body) in &hir.function_bodies {
            let function_info = &self.functions[key];
            let proto = &hir.function_prototypes[key];
            let builder = Builder::new(&function_info.llvm_ref);

            let frame_info = self.create_frame(&body.declared, &builder, None);

            for param in &proto.params {
                let param_ptr = Self::get_name_ptr(&param.decl, &builder, &frame_info);
                builder.store(&param_ptr, &function_info.params[&param.name]);
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
        let exit_code = builder.call(None, llvm::Types::int(32), Rc::clone(&main_info.llvm_ref).to_value(), vec![
            llvm::Types::null().to_value(),
            llvm::Types::null().to_value()
        ]);
        builder.ret(exit_code.to_value());
    }

    fn create_frame<'a>(&self, declared: &Vec<NameKey>, builder: &Builder, parent: Option<&'a FrameInfo<'a>>) -> FrameInfo<'a> {
        let mut items = Vec::new();
        let mut map = HashMap::new();
        items.push(llvm::Types::ptr());   // the parent frame pointer
        for decl in declared {
            map.insert(*decl, items.len());
            let ty = self.generate_type(&self.hir.type_of_name(*decl));
            items.push(ty);
        }
        let frame_ty = llvm::Types::struct_(items);
        let llvm_ref = builder.alloca(Some(self.gen_mangled("frame")), Rc::clone(&frame_ty)).to_value();
        let parent_ptr = builder.gep(None, Rc::clone(&frame_ty), Rc::clone(&llvm_ref), vec![
            GEPIndex::ConstantIndex(0),
            GEPIndex::ConstantIndex(0)
        ]).to_value();
        if let Some(parent_frame) = parent {
            builder.store(&parent_ptr, &Rc::clone(&parent_frame.llvm_ref));
        } else {
            builder.store(&parent_ptr, &llvm::Types::null().to_value());
        }
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
                let frame = self.create_frame(declared, builder, Some(frame));
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

                builder.call(None, self.generate_type(&ret), func.to_value(), args).to_value()
            }
            _ => todo!()
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
            _ => panic!("{:?}", ty)
        }
    }
}

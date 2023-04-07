use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use slotmap::SecondaryMap;
use crate::{hir, llvm};
use crate::hir::{FunctionKey, MayBreak, NameKey, StructKey};
use crate::llvm::{Builder, GEPIndex};

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
}

// struct NameInfo {
//     llvm_ref: llvm::ValueRef,
// }

struct Codegen<'a> {
    llvm: llvm::Module,
    hir: &'a hir::HIR<'a>,

    structs: SecondaryMap<StructKey, StructInfo>,
    functions: SecondaryMap<FunctionKey, FunctionInfo>,
    // names: SecondaryMap<NameKey, NameInfo>,
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
            // names: SecondaryMap::new(),
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
            let params: Vec<llvm::Parameter> = proto.params.iter().map(|p| llvm::Parameter { name: p.name.clone(), typ: self.generate_type(&p.typ) } ).collect();

            let func_ref = self.llvm.add_function(&name, ret, params);

            self.functions.insert(key, FunctionInfo { llvm_ref: func_ref });
        }

        for (key, body) in &hir.function_bodies {
            // let proto = &hir.function_prototypes[key];
            let func_ref = &self.functions[key].llvm_ref;
            let builder = Builder::new(func_ref);

            let frame_info = self.create_frame(&body.declared, &builder, None);

            let res = self.generate_expr(&body.body, &builder, &frame_info);
            if !body.body.does_break() {
                builder.ret(res);
            }
        }

        let main_key = hir.main_function.unwrap();
        let main_info = &self.functions[main_key];
        let main = self.llvm.add_function("main", llvm::Types::int(32), vec![]);
        let builder = Builder::new(&main);
        let exit_code = builder.call(None, llvm::Types::int(32), Rc::clone(&main_info.llvm_ref).as_value(), vec![]);
        builder.ret(exit_code.as_value());
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
        let llvm_ref = builder.alloca(Some(self.gen_mangled("frame")), Rc::clone(&frame_ty)).as_value();
        let parent_ptr = builder.gep(None, Rc::clone(&frame_ty), Rc::clone(&llvm_ref), vec![
            GEPIndex::ConstantIndex(0),
            GEPIndex::ConstantIndex(0)
        ]).as_value();
        if let Some(parent_frame) = parent {
            builder.store(parent_ptr, Rc::clone(&parent_frame.llvm_ref));
        } else {
            builder.store(parent_ptr, llvm::Types::null().to_value());
        }
        FrameInfo { ty: frame_ty, indices: map, parent, llvm_ref }
    }

    fn get_name_ptr(&mut self, name: &NameKey, builder: &Builder, frame: &FrameInfo) -> llvm::ValueRef {
        let mut curr = frame;
        loop {
            match curr.indices.get(name) {
                Some(index) => {
                    return builder.gep(None, Rc::clone(&curr.ty), Rc::clone(&frame.llvm_ref), vec![
                        GEPIndex::ArrayIndex(llvm::Types::int_constant(64, 0).to_value()),
                        GEPIndex::ConstantIndex(*index as u32)
                    ]).as_value();
                }
                None => {
                    curr = frame.parent.unwrap();
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
                let ptr = self.get_name_ptr(decl, builder, frame);
                let ty = self.generate_type(&self.hir.type_of_name(*decl));
                builder.load(None, ty, ptr).as_value()
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
                let ptr = self.get_name_ptr(decl, builder, frame);
                builder.store(ptr, expr);
            }
        }
    }

    fn generate_type(&self, ty: &hir::Type) -> llvm::TypeRef {
        match ty {
            hir::Type::Integer { bits } => llvm::Types::int(*bits),
            hir::Type::Boolean => llvm::Types::int(1),
            _ => todo!()
        }
    }
}

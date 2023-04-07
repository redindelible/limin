use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use semver::Op;
use slotmap::SecondaryMap;
use crate::{hir, llvm};
use crate::hir::{FunctionKey, HIR, NameKey, StructKey};
use crate::llvm::{BasicBlockRef, GEPIndex, InstrRef, TypeRef, ValueRef};

pub fn generate_llvm(hir: HIR) -> llvm::Module {
    let mut gen = Codegen::new(hir.name.clone());
    gen.generate(&hir);
    gen.llvm
}

struct StructInfo {
    llvm_ref: llvm::StructRef,
    fields: HashMap<String, usize>
}

struct FunctionInfo {
    llvm_ref: llvm::FunctionRef,
}

struct NameInfo {
    llvm_ref: llvm::ValueRef,
}

struct Codegen {
    llvm: llvm::Module,

    structs: SecondaryMap<StructKey, StructInfo>,
    functions: SecondaryMap<FunctionKey, FunctionInfo>,
    names: SecondaryMap<NameKey, NameInfo>,
    mangle: RefCell<u32>,
}

fn inc(item: &mut u32) -> u32 {
    let val = *item;
    *item += 1;
    val
}

struct FrameInfo<'a> {
    ty: llvm::TypeRef,
    indices: HashMap<NameKey, usize>,
    parent: Option<&'a FrameInfo<'a>>,
    llvm_ref: ValueRef
}

impl Codegen {
    fn new(name: String) -> Codegen {
        Codegen {
            llvm: llvm::Module::new(name),
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

    fn generate(&mut self, hir: &HIR) {
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
            let proto = &hir.function_prototypes[key];
            let func_ref = &self.functions[key].llvm_ref;
            let entry = func_ref.add_block("entry".into());

            let frame_info = self.create_frame(hir, &body.declared, &entry, None);

            let res = self.generate_expr(&body.body, &entry, &frame_info);

            entry.ret(llvm::Types::int_constant(32, 0).to_value());
        }

        let main_key = hir.main_function.unwrap();
        let main_info = &self.functions[main_key];
        let main = self.llvm.add_function("main", llvm::Types::int(32), vec![]);
        let block = main.add_block("entry".into());
        let exit_code = block.call(None, llvm::Types::int(32), Rc::clone(&main_info.llvm_ref).as_value(), vec![]);
        block.ret(exit_code.as_value());
    }

    fn create_frame<'a>(&self, hir: &HIR, declared: &Vec<NameKey>, block: &BasicBlockRef, parent: Option<&'a FrameInfo<'a>>) -> FrameInfo<'a> {
        let mut items = Vec::new();
        let mut map = HashMap::new();
        items.push(llvm::Types::ptr());   // the parent frame pointer
        for decl in declared {
            map.insert(*decl, items.len());
            let ty = self.generate_type(&hir.type_of_name(*decl));
            items.push(ty);
        }
        let frame_ty = llvm::Types::struct_(items);
        let llvm_ref = block.alloca(Some(self.gen_mangled("frame")), Rc::clone(&frame_ty));
        FrameInfo { ty: frame_ty, indices: map, parent, llvm_ref: llvm_ref.as_value() }
    }

    fn generate_expr(&mut self, expr: &hir::Expr, block: &BasicBlockRef, frame: &FrameInfo) -> llvm::ValueRef {
        match expr {
            hir::Expr::Integer { num, .. } => {
                llvm::Types::int_constant(32, *num).to_value()
            }
            hir::Expr::Name { decl, .. } => {
                let mut curr = frame;
                loop {
                    match curr.indices.get(decl) {
                        Some(index) => {
                            return block.gep(None, Rc::clone(&curr.ty), Rc::clone(&frame.llvm_ref), vec![
                                GEPIndex::ArrayIndex(llvm::Types::int_constant(64, 0).to_value()),
                                GEPIndex::StructIndex(*index as u32)
                            ]).as_value();
                        }
                        None => {
                            curr = frame.parent.unwrap();
                        }
                    }
                }
            }
            _ => todo!()
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

use std::collections::HashMap;
use std::ops::{Add, AddAssign, Range, RangeInclusive};
use std::rc::Rc;
use slotmap::SecondaryMap;
use crate::{hir, llvm};
use crate::hir::{HIR, StructKey};
use crate::llvm::{Module, StructRef};

pub fn generate_llvm(hir: HIR) -> Module {
    let mut gen = Codegen::new(hir);
    gen.generate();
    gen.llvm
}

struct StructInfo {
    llvm_ref: StructRef,
    fields: HashMap<String, usize>
}

struct Codegen<'a> {
    hir: HIR<'a>,
    llvm: Module,

    structs: SecondaryMap<StructKey, StructInfo>,
    mangle: u32,
}

fn inc(item: &mut u32) -> u32 {
    let val = *item;
    *item += 1;
    val
}

impl<'a> Codegen<'a> {
    fn new(hir: HIR<'a>) -> Codegen<'a> {
        let name = hir.name.clone();
        Codegen {
            hir,
            llvm: Module::new(name),
            structs: SecondaryMap::new(),
            mangle: 0
        }
    }

    fn generate(&mut self) {
        for (key, _) in &self.hir.structs {
            let name = &self.hir.structs[key].name;
            let num = inc(&mut self.mangle);
            let name = format!("struct_{}_{:>08X}", name, num);
            let struct_ref = self.llvm.types.add_struct(Some(&name));
            let info = StructInfo { llvm_ref: struct_ref, fields: HashMap::new() };
            self.structs.insert(key, info);
        }

        for (key, struct_) in &self.hir.structs {
            let mut fields = Vec::new();
            for field in struct_.fields.values() {
                self.structs[key].fields.insert(field.name.clone(), fields.len());
                fields.push(self.generate_type(&field.typ));
            }
            self.structs[key].llvm_ref.set_fields(fields);
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

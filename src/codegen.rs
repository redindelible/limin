use std::collections::HashMap;
use std::ops::{Add, AddAssign, Range, RangeInclusive};
use std::rc::Rc;
use slotmap::SecondaryMap;
use crate::{hir, llvm};
use crate::hir::{FunctionKey, HIR, StructKey};
use crate::llvm::{FunctionRef, Module, StructRef, Types};

pub fn generate_llvm(hir: HIR) -> Module {
    let mut gen = Codegen::new(hir);
    gen.generate();
    gen.llvm
}

struct StructInfo {
    llvm_ref: StructRef,
    fields: HashMap<String, usize>
}

struct FunctionInfo {
    llvm_ref: FunctionRef,
}

struct Codegen<'a> {
    hir: HIR<'a>,
    llvm: Module,

    structs: SecondaryMap<StructKey, StructInfo>,
    functions: SecondaryMap<FunctionKey, FunctionInfo>,
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
            functions: SecondaryMap::new(),
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

        for (key, _ ) in &self.hir.function_prototypes {
            let proto = &self.hir.function_prototypes[key];

            let name = &proto.name;
            let num = inc(&mut self.mangle);
            let name = format!("function_{}_{:>08X}", name, num);

            let ret = self.generate_type(&proto.ret);
            let params: Vec<llvm::Parameter> = proto.params.iter().map(|p| llvm::Parameter { name: p.name.clone(), typ: self.generate_type(&p.typ) } ).collect();

            let func_ref = self.llvm.add_function(&name, ret, params);

            self.functions.insert(key, FunctionInfo { llvm_ref: func_ref });
        }

        let main_key = self.hir.main_function.unwrap();
        let main = self.llvm.add_function("main", Types::int(32), vec![]);
        let block = main.add_block("entry".into());
        block.ret(Types::int_constant(32, 0).to_value());
    }

    fn generate_type(&self, ty: &hir::Type) -> llvm::TypeRef {
        match ty {
            hir::Type::Integer { bits } => llvm::Types::int(*bits),
            hir::Type::Boolean => llvm::Types::int(1),
            _ => todo!()
        }
    }
}

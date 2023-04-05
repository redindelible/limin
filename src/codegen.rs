use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use slotmap::SecondaryMap;
use crate::{hir, llvm};
use crate::hir::{FunctionKey, HIR, NameKey, StructKey};

pub fn generate_llvm(hir: HIR) -> llvm::Module {
    let mut gen = Codegen::new(hir);
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

struct NameInfo {
    llvm_ref: llvm::ValueRef,
}

struct Codegen<'a> {
    hir: HIR<'a>,
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

struct FrameInfo {
    ty: llvm::TypeRef,
    indices: HashMap<NameKey, usize>
}

impl<'a> Codegen<'a> {
    fn new(hir: HIR<'a>) -> Codegen<'a> {
        let name = hir.name.clone();
        Codegen {
            hir,
            llvm: llvm::Module::new(name),
            structs: SecondaryMap::new(),
            functions: SecondaryMap::new(),
            names: SecondaryMap::new(),
            mangle: RefCell::new(0)
        }
    }

    fn gen_mangled(&self, prefix: &'static str, name: &str) -> String {
        let num = *self.mangle.borrow();
        *self.mangle.borrow_mut() += 1;
        format!("{prefix}_{name}_{:>08X}", num)
    }

    fn generate(&mut self) {
        for (key, _) in &self.hir.structs {
            let name = self.gen_mangled("struct", &self.hir.structs[key].name);
            let struct_ref = self.llvm.types.add_struct(name);
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

            let name = self.gen_mangled("function", &proto.name);

            let ret = self.generate_type(&proto.ret);
            let params: Vec<llvm::Parameter> = proto.params.iter().map(|p| llvm::Parameter { name: p.name.clone(), typ: self.generate_type(&p.typ) } ).collect();

            let func_ref = self.llvm.add_function(&name, ret, params);

            self.functions.insert(key, FunctionInfo { llvm_ref: func_ref });
        }

        for (key, body) in &self.hir.function_bodies {
            let proto = &self.hir.function_prototypes[key];
            let func_ref = &self.functions[key].llvm_ref;
            let entry = func_ref.add_block("entry".into());

            let frame_info = self.create_frame_type(&body.declared);

            let frame = entry.alloca(self.gen_mangled("frame", &proto.name), Rc::clone(&frame_info.ty));
            entry.ret(llvm::Types::int_constant(32, 0).to_value());
        }

        let main_key = self.hir.main_function.unwrap();
        let main_info = &self.functions[main_key];
        let main = self.llvm.add_function("main", llvm::Types::int(32), vec![]);
        let block = main.add_block("entry".into());
        let exit_code = block.call("exit_code".into(), llvm::Types::int(32), Rc::clone(&main_info.llvm_ref).as_value(), vec![]);
        block.ret(exit_code.as_value());
    }

    fn create_frame_type(&self, declared: &Vec<NameKey>) -> FrameInfo {
        let mut items = Vec::new();
        let mut map = HashMap::new();
        items.push(llvm::Types::ptr());   // the parent frame pointer
        for decl in declared {
            map.insert(*decl, items.len());
            let ty = self.generate_type(&self.hir.type_of_name(*decl));
            items.push(ty);
        }
        FrameInfo { ty: llvm::Types::struct_(items), indices: map }
    }

    fn generate_type(&self, ty: &hir::Type) -> llvm::TypeRef {
        match ty {
            hir::Type::Integer { bits } => llvm::Types::int(*bits),
            hir::Type::Boolean => llvm::Types::int(1),
            _ => todo!()
        }
    }
}

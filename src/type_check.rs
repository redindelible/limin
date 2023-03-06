use std::collections::HashMap;
use std::sync::Arc;
use async_scoped::AsyncScope;
use async_std::sync::Mutex;
use slotmap::{new_key_type, SlotMap};
use crate::ast::{AST, TopLevel};
use crate::hir::{HIR, NameDecl, TypeDecl, TypeKey, NameKey, Struct, StructKey, Type};

struct Progress {
    collect_type_names: async_std::sync::Barrier,
    collect_types: async_std::sync::Barrier,
}

new_key_type! {
    struct NamespaceKey;
}

struct Namespace {
    names: HashMap<String, NameKey>,
    types: HashMap<String, Type>,
    namespaces: HashMap<String, NamespaceKey>
}

struct TypeCheck<'s> {
    progress: Progress,
    structs: Mutex<SlotMap<StructKey, Struct<'s>>>,
    namespaces: Mutex<SlotMap<NamespaceKey, Namespace>>
}

impl Progress {
    fn new(num_types: usize, num_funcs: usize) -> Self {
        Self {
            collect_type_names: async_std::sync::Barrier::new(num_types),
            collect_types: async_std::sync::Barrier::new(num_types),
        }
    }
}


impl<'s> TypeCheck<'s> {
    pub fn check(ast: AST<'s>) -> () {
        let mut num_types = 0;
        let mut num_funcs = 0;
        for file in &ast.files {
            for top_level in &file.top_levels {
                match top_level {
                    TopLevel::Struct { items, .. } => {
                        num_types += 1;
                    }
                    TopLevel::Function { .. } => {
                        num_funcs += 1;
                    }
                }
            }
        }

        let checker = TypeCheck::new(num_types, num_funcs);
        let namespace = Mutex::new(Namespace { ..Default::default() });

        AsyncScope::scope_and_block(|scope| {
            for file in ast.files {
                for top_level in file.top_levels {
                    match top_level {
                        TopLevel::Struct { .. } => scope.spawn(handle_struct(&checker, top_level, &namespace)),
                        TopLevel::Function { .. } => scope.spawn(handle_function(&checker, top_level))
                    }
                }
            }
        });
    }
}

async fn handle_struct<'s>(checker: &TypeCheck<'s>, struct_: TopLevel<'s>, global: &Mutex<Namespace>) {
    let TopLevel::Struct { name, items } = struct_ else { panic!("arg must be a struct") };

    let key;
    {
        let struct_ = Struct { name: name.clone(), fields: HashMap::new() };
        let mut structs = checker.structs.lock().await;
        key = structs.insert(struct_);
    }

    checker.progress.collect_type_names.wait().await;
}

async fn handle_function<'s>(checker: &Mutex<TypeCheck<'s>>, func: TopLevel<'s>) {

}


#[cfg(test)]
mod test {

}

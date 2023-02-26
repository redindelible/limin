use std::sync::Arc;
use async_scoped::AsyncScope;
use async_std::sync::Mutex;
use slotmap::SlotMap;
use crate::ast::{AST, TopLevel};
use crate::hir::{HIR, TypeDecl, TypeKey};

struct Progress {
    collect_type_names: async_std::sync::Barrier,
    collect_types: async_std::sync::Barrier,
}

struct Namespace {

}

struct TypeCheck<'s> {
    types: SlotMap<TypeKey, TypeDecl<'s>>,

    phantom: std::marker::PhantomData<&'s ()>
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

        let checker = Mutex::new(TypeCheck::new(num_types, num_funcs));

        AsyncScope::scope_and_block(|scope| {
            for file in ast.files {
                for top_level in file.top_levels {
                    match top_level {
                        TopLevel::Struct { .. } => scope.spawn(handle_struct(&checker, top_level)),
                        TopLevel::Function { .. } => scope.spawn(handle_function(&checker, top_level))
                    }
                }
            }
        });
    }
}

async fn handle_struct<'s>(checker: &Mutex<TypeCheck<'s>>, struct_: TopLevel<'s>) {
    let TopLevel::Struct { name, items } = struct_ else { panic!("arg must be a struct") };
    checker.lock().await.collect_type_names.wait().await;
}

async fn handle_function<'s>(checker: &Mutex<TypeCheck<'s>>, func: TopLevel<'s>) {

}


#[cfg(test)]
mod test {

}

use std::cell::{RefCell};
use std::collections::HashMap;
use std::ffi::c_char;
use std::sync::Arc;
use async_scoped::AsyncScope;
use async_std::sync::Mutex;
use slotmap::{new_key_type, SlotMap};
use crate::ast;
use crate::hir::{HIR, NameDecl, TypeKey, NameKey, Struct, StructKey, Type, StructField};

struct Progress {
    collect_type_names: async_std::sync::Barrier,
    collect_types: async_std::sync::Barrier,
}

new_key_type! {
    struct NamespaceKey;
}

#[derive(Default)]
struct Namespace {
    names: HashMap<String, NameKey>,
    types: HashMap<String, Type>,
    namespaces: HashMap<String, NamespaceKey>
}

impl Namespace {
    fn insert_type(&mut self, name: String, typ: Type) {
        self.types.insert(name, typ);
    }

    fn get_type(&self, name: &str) -> Option<Type> {
        self.types.get(name).copied()
    }
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
    fn new(num_types: usize, num_funcs: usize) -> Self {
        TypeCheck {
            progress: Progress::new(num_types, num_funcs),
            structs: Mutex::new(SlotMap::with_key()),
            namespaces: Mutex::new(SlotMap::with_key())
        }
    }

    async fn wait_type_names_collected(&self) {
        self.progress.collect_type_names.wait().await;
    }

    async fn wait_types_collected(&self) {
        self.progress.collect_types.wait().await;
    }

    async fn insert_namespace(&self, namespace: Namespace) -> NamespaceKey {
        self.namespaces.lock().await.insert(namespace)
    }

    pub fn check(ast: ast::AST<'s>) -> () {
        let mut num_types = 0;
        let mut num_funcs = 0;
        for file in &ast.files {
            for top_level in &file.top_levels {
                match top_level {
                    ast::TopLevel::Struct { items, .. } => {
                        num_types += 1;
                    }
                    ast::TopLevel::Function { .. } => {
                        num_funcs += 1;
                    }
                }
            }
        }

        let mut checker = TypeCheck::new(num_types, num_funcs);
        let global = checker.namespaces.get_mut().insert(Namespace { ..Default::default() });

        AsyncScope::scope_and_block(|scope| {
            for file in ast.files {
                for top_level in file.top_levels {
                    match top_level {
                        ast::TopLevel::Struct { .. } => scope.spawn(checker.handle_struct(top_level, global)),
                        ast::TopLevel::Function { .. } => scope.spawn(handle_function(&checker, top_level))
                    }
                }
            }
        });
    }

    async fn handle_struct(&self, struct_: ast::TopLevel<'s>, global: NamespaceKey) {
        let ast::TopLevel::Struct { name, items } = struct_ else { panic!("arg must be a struct") };

        let key = {
            let struct_ = Struct { name: name.clone(), fields: HashMap::new() };
            let key = self.structs.lock().await.insert(struct_);
            self.namespaces.lock().await[global].insert_type(name.clone(), Type::Struct { struct_: key });
            key
        };

        self.wait_type_names_collected().await;

        {
            let ir_struct = &mut self.structs.lock().await[key];
            for item in &items {
                if let ast::StructItem::Field { name, typ, loc } = item {
                    let resolved_type = self.resolve_type(typ.as_ref(), global).await;
                    ir_struct.fields.insert(name.clone(), StructField {
                        name: name.clone(), typ: resolved_type, loc: *loc
                    });
                }
            }
        }

        self.wait_types_collected().await;
    }

    async fn resolve_type(&self, ast_type: &ast::Type<'s>, in_namespace: NamespaceKey) -> Type {
        match ast_type {
            ast::Type::Name { name, .. } => {
                let ns = &self.namespaces.lock().await[in_namespace];
                ns.types[name]
            }
        }
    }
}

async fn handle_function<'s>(checker: &TypeCheck<'s>, func: ast::TopLevel<'s>) {

}


#[cfg(test)]
mod test {

}

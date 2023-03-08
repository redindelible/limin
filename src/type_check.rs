use std::collections::HashMap;
use async_scoped::AsyncScope;
use async_recursion::async_recursion;
use async_std::sync::Mutex;
use slotmap::{new_key_type, SecondaryMap, SlotMap};
use crate::ast;
use crate::hir::{HIR, NameKey, NameInfo, TypeKey, Struct, StructKey, Type, StructField, FunctionKey, Parameter, FunctionPrototype, FunctionBody, Expr, LogicOp};
use crate::source::Location;

#[derive(Debug)]
enum TypeCheckError<'a> {
    CouldNotResolveName(String, Location<'a>),
    CouldNotResolveType(String, Location<'a>),
    IncompatibleTypes(Type, Type, Location<'a>),
    NotEnoughInfoToInfer(Location<'a>)
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

struct Progress {
    collect_type_names: async_std::sync::Barrier,
    collect_types: async_std::sync::Barrier,
    collect_function_signatures: async_std::sync::Barrier
}

struct TypeCheck<'s> {
    progress: Progress,
    hir: Mutex<HIR<'s>>,
    namespaces: Mutex<SlotMap<NamespaceKey, Namespace>>,

    errors: Mutex<Vec<TypeCheckError<'s>>>
}

impl Progress {
    fn new(num_types: usize, num_funcs: usize) -> Self {
        Self {
            collect_type_names: async_std::sync::Barrier::new(num_types),
            collect_types: async_std::sync::Barrier::new(num_types + num_funcs),
            collect_function_signatures: async_std::sync::Barrier::new(num_funcs),
        }
    }
}

type ExpectedType = Option<Type>;

impl<'s> TypeCheck<'s> {
    fn new(num_types: usize, num_funcs: usize) -> Self {
        TypeCheck {
            progress: Progress::new(num_types, num_funcs),
            hir: Default::default(),
            namespaces: Default::default(),
            errors: Default::default()
        }
    }

    async fn wait_type_names_collected(&self) {
        self.progress.collect_type_names.wait().await;
    }

    async fn wait_types_collected(&self) {
        self.progress.collect_types.wait().await;
    }

    async fn wait_functions_signatures(&self) {
        self.progress.collect_function_signatures.wait().await;
    }

    async fn insert_namespace(&self, namespace: Namespace) -> NamespaceKey {
        self.namespaces.lock().await.insert(namespace)
    }

    async fn add_type_to_ns(&self, ns: NamespaceKey, name: String, typ: Type) {
        self.namespaces.lock().await[ns].types.insert(name, typ);
    }

    async fn add_name_to_ns(&self, ns: NamespaceKey, name: String, name_key: NameKey) {
        self.namespaces.lock().await[ns].names.insert(name, name_key);
    }

    async fn get_type_from_ns(&self, ns: NamespaceKey, name: &str) -> Option<Type> {
        self.namespaces.lock().await[ns].types.get(name).cloned()
    }

    async fn get_name_from_ns(&self, ns: NamespaceKey, name: &str) -> Option<NameKey> {
        self.namespaces.lock().await[ns].names.get(name).cloned()
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
                        ast::TopLevel::Function { .. } => scope.spawn(checker.handle_function(top_level, global))
                    }
                }
            }
        });
    }

    async fn handle_struct(&self, struct_: ast::TopLevel<'s>, global: NamespaceKey) {
        let ast::TopLevel::Struct { name, items } = struct_ else { panic!("arg must be a struct") };

        let key = {
            let struct_ = Struct { name: name.clone(), fields: HashMap::new() };
            let key = self.hir.lock().await.add_struct(struct_);
            self.add_type_to_ns(global, name.clone(), Type::Struct { struct_: key }).await;
            key
        };

        self.wait_type_names_collected().await;

        {
            let mut hir = self.hir.lock().await;
            let ir_struct = hir.get_struct(key);
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


    async fn handle_function(&self, func: ast::TopLevel<'s>, global: NamespaceKey) {
        let ast::TopLevel::Function { name, parameters, return_type, body } = func else { panic!("arg must be a struct") };

        self.wait_types_collected().await;

        let ret = match return_type {
            Some(t) => self.resolve_type(&t, global).await,
            None => Type::Unit
        };
        let params = {
            let mut params = Vec::new();
            for param in parameters {
                let param_ = Parameter { name: name.clone(), typ: self.resolve_type(&param.typ, global).await, loc: param.loc };
                params.push(param_);
            };
            params
        };

        let signature = Type::Function { params: params.iter().map(|p| Box::new(p.typ.clone())).collect(), ret: Box::new(ret.clone()) };
        let prototype = FunctionPrototype { name: name.clone(), params, ret, sig: signature };

        let key = self.hir.lock().await.add_prototype(prototype);
        let name_key = self.hir.lock().await.add_name(NameInfo::Function { func: key });
        self.add_name_to_ns(global, name.clone(), name_key).await;

        self.wait_functions_signatures().await;
    }

    async fn resolve_type(&self, ast_type: &ast::Type<'s>, in_namespace: NamespaceKey) -> Type {
        match ast_type {
            ast::Type::Name { name, loc } => {
                match self.get_type_from_ns(in_namespace, name).await {
                    Some(t) => t,
                    None => {
                        self.errors.lock().await.push(TypeCheckError::CouldNotResolveType(name.clone(), *loc));
                        Type::Errored
                    }
                }
            }
        }
    }

    async fn unify_types(&self, dest_type: ExpectedType, source_type: ExpectedType, loc: Location<'s>) -> (Type, Type) {
        match (dest_type, source_type) {
            (Some(a), Some(Type::Errored)) => (a, Type::Errored),
            (Some(Type::Errored), Some(a)) => (Type::Errored, a),

            (Some(Type::Integer { bits: dest_bits }), Some(Type::Integer { bits: source_bits })) => {
                if dest_bits < source_bits {

                }
                (Type::Integer { bits: dest_bits }, Type::Integer { bits: source_bits })
            }

            (Some(a), Some(b)) => {
                self.errors.lock().await.push(TypeCheckError::IncompatibleTypes(a.clone(), b.clone(), loc));
                (a, b)
            }

            (None, Some(a)) => (a.clone(), a),
            (Some(a), None) => (a.clone(), a),

            (None, None) => {
                self.errors.lock().await.push(TypeCheckError::NotEnoughInfoToInfer(loc));
                (Type::Errored, Type::Errored)
            }
        }
    }

    #[async_recursion]
    async fn resolve_expr(&self, ast_expr: &ast::Expr<'s>, expected_type: ExpectedType, ns: NamespaceKey) -> Expr {
        match ast_expr {
            ast::Expr::Name { name, loc } => {
                let matched = self.get_name_from_ns(ns, name).await;
                match matched {
                    Some(name_key) => {
                        let source_type = self.hir.lock().await.type_of_name(name_key);
                        self.unify_types(expected_type, Some(source_type), *loc).await;
                        Expr::Name { decl: name_key, loc: *loc }
                    },
                    None => {
                        self.errors.lock().await.push(TypeCheckError::CouldNotResolveName(name.clone(), *loc));
                        Expr::Errored { loc: *loc }
                    }
                }
            }
            ast::Expr::Integer { number, loc } => {
                self.unify_types(expected_type, Some(Type::Integer { bits: 64 }), *loc).await;
                Expr::Integer { num: *number, loc: *loc}
            }
            ast::Expr::BinOp { left, op, right, loc } => {
                let left = Box::new(self.resolve_expr(left, None, ns).await);
                let right = Box::new(self.resolve_expr(right, None, ns).await);
                self.unify_types(expected_type, Some(Type::Boolean), *loc).await;
                match op {
                    ast::BinOp::LessThan => Expr::LogicBinOp { left, right, op: LogicOp::LessThan },
                    ast::BinOp::GreaterThan => Expr::LogicBinOp { left, right, op: LogicOp::GreaterThan }
                }
            }
            _ => todo!()
        }
    }
}


#[cfg(test)]
mod test {

}

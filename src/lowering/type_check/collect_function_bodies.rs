use std::collections::HashMap;
use indexmap::IndexMap;
use crate::parsing::ast;
use crate::lowering::hir::{self, Coercion, FunctionType, InferenceVariableKey, StructType, TraitType, Type};
use crate::lowering::type_check as tc;
use crate::lowering::type_check::ResolveResult;
use crate::source::{HasLoc, Location};
use crate::util::KeyMap;


pub(super) fn collect_function_bodies<'a, 'b>(mut checker: tc::TypeCheck<'a>, collected: tc::collect_functions::CollectedPrototypes<'a, 'b>) -> ResolveResult<'a, hir::HIR<'a>> {
    let tc::collect_functions::CollectedPrototypes {
        functions,
        structs,
        traits,
        impls,
        methods: method_info,
        main_function,
        ..
    } = &collected;

    let mut hir_functions = KeyMap::new();
    for (func_key, func_info) in functions {
        let ret = func_info.ret.clone();
        let mut context = ResolveContext::create_for_function(&mut checker, &collected, Some(ret.clone()), func_info.ns);
        let body = context.resolve_block(&func_info.ast_func.body, Some(ret), func_info.ns);

        let mut parameters = Vec::new();
        for (param_name, param_info) in &func_info.params {
            parameters.push(hir::Parameter {
                name: param_name.clone(),
                typ: param_info.ty.clone(),
                decl: param_info.decl,
                loc: param_info.loc
            });
        }

        hir_functions.insert(func_key, hir::Function {
            name: func_info.ast_func.name.clone(),
            decl: func_info.name_key,
            type_params: func_info.type_parameters.values().copied().collect(),
            params: parameters,
            ret: func_info.ret.clone(),
            body,
            loc: func_info.ast_func.loc
        });
    }

    let mut hir_structs = KeyMap::new();
    for (struct_key, struct_info) in structs {
        let mut fields = IndexMap::new();
        for (field_name, field_info) in &struct_info.fields {
            fields.insert(field_name.clone(), hir::StructField {
                name: field_name.clone(),
                typ: field_info.ty.clone(),
                loc: field_info.loc
            });
        }

        hir_structs.insert(struct_key, hir::Struct {
            name: struct_info.ast_struct.name.clone(),
            type_params: struct_info.type_parameters.values().copied().collect(),
            super_struct: None,
            fields,
            loc: struct_info.ast_struct.loc
        });
    }

    let mut hir_traits = KeyMap::new();
    for (trait_key, trait_info) in traits {
        hir_traits.insert(trait_key, hir::Trait {
            name: trait_info.ast_trait.name.clone(),
            type_params: trait_info.type_parameters.iter().map(|(_, &key)| key).collect(),
            methods: trait_info.methods.iter().map(|(name, method)| {
                (name.clone(), hir::MethodPrototype {
                    has_self: method.has_self,
                    params: method.params.clone(),
                    ret: method.ret.clone(),
                    loc: method.loc
                })
            }).collect()
        });
    }

    let mut hir_impls= KeyMap::new();
    let mut hir_methods = KeyMap::new();
    for (impl_key, impl_) in impls {
        let mut methods = HashMap::new();
        for (method_name, method_key) in &impl_.methods {
            let method_info = &method_info[*method_key];
            let ret = method_info.ret.clone();
            let mut context = ResolveContext::create_for_function(&mut checker, &collected, Some(ret.clone()), method_info.ns);
            let body = context.resolve_block(&method_info.ast_method.body, Some(ret.clone()), method_info.ns);

            let mut parameters = Vec::new();
            for (param_name, param_info) in &method_info.params {
                parameters.push(hir::Parameter {
                    name: param_name.clone(),
                    typ: param_info.ty.clone(),
                    decl: param_info.decl,
                    loc: param_info.loc
                });
            }

            let method_key = hir_methods.add(hir::Method {
                in_impl: impl_key,
                name: method_name.clone(),
                type_params: method_info.type_parameters.values().copied().collect(),
                maybe_self: method_info.maybe_self,
                params: parameters,
                ret,
                body,
                loc: method_info.ast_method.loc
            });

            methods.insert(method_name.clone(), method_key);
        }

        hir_impls.insert(impl_key, hir::Impl {
            impl_trait: None,
            bounds: vec![],
            for_type: impl_.for_type.clone(),
            methods,
            loc: impl_.loc
        });
    }


    let tc::TypeCheck { name, names, type_parameters, inference_variables, errors, .. } = checker;

    let &Some(main_function) = main_function else {
        return ResolveResult::Failure(errors);
    };
    if !errors.is_empty() {
        return ResolveResult::Failure(errors);
    }

    let hir = hir::HIR {
        name,
        main_function,
        names,
        type_parameters,
        inference_variables,
        structs: hir_structs,
        traits: hir_traits,
        functions: hir_functions,
        impls: hir_impls,
        methods: hir_methods
    };

    return ResolveResult::Success(hir)
}


type ExpectedType = Option<Type>;

struct AnnotatedExpr<'a> {
    expr: hir::Expr<'a>,
    ty: Type,
    always_diverges: bool
}

impl AnnotatedExpr<'_> {
    fn new_errored(loc: Location, always_diverges: bool) -> AnnotatedExpr {
        AnnotatedExpr { expr: hir::Expr::Errored { loc }, ty: Type::Errored, always_diverges }
    }
}

struct AnnotatedStmt<'a> {
    stmt: hir::Stmt<'a>,
    always_diverges: bool
}


struct ResolveContext<'a, 'b> where 'a: 'b {
    checker: &'b mut tc::TypeCheck<'a>,
    types: &'b tc::CollectedTypes<'a, 'b>,
    prototypes: &'b tc::CollectedPrototypes<'a, 'b>,

    expected_return: Option<Type>,
    namespace: tc::NamespaceKey,
    level: usize,

    return_types: Vec<Type>
}

impl<'a, 'b> ResolveContext<'a, 'b> where 'a: 'b  {
    fn create_for_function(checker: &'b mut tc::TypeCheck<'a>, prototypes: &'b tc::CollectedPrototypes<'a, 'b>, expected_return: Option<Type>, namespace: tc::NamespaceKey) -> ResolveContext<'a, 'b> {
        ResolveContext {
            checker, types: &prototypes.types, prototypes, expected_return, namespace,
            level: 0,
            return_types: vec![]
        }
    }

    fn push_error(&mut self, error: tc::TypeCheckError<'a>) {
        self.checker.push_error(error)
    }

    fn resolve_name(&self, name: &str) -> Option<tc::NameKey> {
        self._resolve_name(self.namespace, name)
    }

    fn _resolve_name(&self, ns: tc::NamespaceKey, name: &str) -> Option<tc::NameKey> {
        match self.checker.namespaces[ns].names.get(name) {
            Some(n) => Some(*n),
            None => self.checker.namespaces[ns].parent.and_then(|p| self._resolve_name(p, name))
        }
    }

    fn add_local(&mut self, name: impl Into<String>, ty: Type, loc: Location<'a>) -> (tc::NameKey, Option<tc::NameKey>) {
        self.checker.add_local(name, ty, self.level, loc, self.namespace)
    }

    fn resolve_type(&mut self, typ: &ast::Type<'a>) -> Type {
        self.checker.resolve_type(typ, self.namespace, self.types).expect_type(self.checker)
    }

    fn display_type(&self, ty: &Type) -> tc::DisplayType<'a> {
        use tc::DisplayType;
        match ty {
            Type::Unit => DisplayType::Unit,
            Type::Never => DisplayType::Never,
            Type::Boolean => DisplayType::Boolean,
            Type::Errored => DisplayType::Errored,
            Type::SignedInteger(bits) => DisplayType::Integer { is_signed: true, bits: *bits },
            Type::UnsignedInteger(bits) => DisplayType::Integer { is_signed: false, bits: *bits },
            Type::Gc(inner) => DisplayType::Gc(Box::new(self.display_type(inner))),
            Type::Ref(inner) => DisplayType::Ref(Box::new(self.display_type(inner))),
            Type::Struct(StructType(struct_key, variant)) => {
                let struct_info= &self.types.structs[*struct_key];
                DisplayType::Struct {
                    name: struct_info.name.clone(),
                    variant: variant.iter().map(|t| self.display_type(t)).collect(),
                    loc: struct_info.ast_struct.loc
                }
            },
            Type::Trait(TraitType(trait_key, variant)) => {
                let trait_info = &self.types.traits[*trait_key];
                DisplayType::Struct {
                    name: trait_info.name.clone(),
                    variant: variant.iter().map(|t| self.display_type(t)).collect(),
                    loc: trait_info.ast_trait.loc
                }
            }
            Type::Function(FunctionType(params, ret)) => DisplayType::Function {
                params: params.iter().map(|t| self.display_type(t)).collect(),
                ret: Box::new(self.display_type(ret))
            },
            Type::TypeParameter(ty_param_key) => {
                let info = &self.checker.type_parameters[*ty_param_key];
                DisplayType::TypeParameter { name: info.name.clone(), bound: None }
            }
            Type::InferenceVariable(key) => {
                let info = self.checker.query_inference_variable(*key);
                DisplayType::InferenceVariable { name: info.name.clone(), inferred: info.ty.as_ref().map(|ty| Box::new(self.display_type(ty))) }
            }
        }
    }

    #[must_use]
    fn equate_types(&mut self, a: &Type, b: &Type, loc: Location<'a>) -> ResolveResult<'a, ()> {
        let failure = |s: &Self| ResolveResult::Failure(vec![tc::TypeCheckError::IncompatibleTypes { expected: s.display_type(b), got: s.display_type(a), loc }]);

        match (a, b) {
            (Type::Errored, _) | (_, Type::Errored) => ResolveResult::Success(()),
            (Type::Never, Type::Never) => ResolveResult::Success(()),
            (Type::Boolean, Type::Boolean) => ResolveResult::Success(()),
            (Type::Unit, Type::Unit) => ResolveResult::Success(()),
            (Type::SignedInteger(a_bits), Type::SignedInteger(b_bits)) => {
                if a_bits == b_bits {
                    ResolveResult::Success(())
                } else {
                    failure(self)
                }
            },
            (Type::Struct(StructType(a_struct, a_variant)), Type::Struct(StructType(b_struct, b_variant))) => {
                if a_struct != b_struct {
                    return failure(self);
                }
                assert_eq!(a_variant.len(), b_variant.len());
                let mut results = Vec::new();
                for (a_type_param, b_type_param) in a_variant.into_iter().zip(b_variant) {
                    results.push(self.equate_types(a_type_param, b_type_param, loc));
                }
                let result = ResolveResult::collect_results::<()>(results);
                if result.is_failure() {
                    return failure(self);
                }
                return ResolveResult::Success(());
            }
            (Type::TypeParameter(a_key), Type::TypeParameter(b_key)) => {
                if a_key == b_key {
                    return ResolveResult::Success(())
                } else {
                    return failure(self);
                }
            }
            (&Type::InferenceVariable(key), b) => {
                let infer_info = self.checker.query_inference_variable_mut(key);
                if let Some(typ) = &infer_info.ty {
                    let ty = typ.clone();
                    return self.equate_types(&ty, b, loc);
                } else {
                    if let &Type::InferenceVariable(b_key) = b {
                        if b_key == key {
                            return ResolveResult::Success(());
                        }
                    }
                    infer_info.ty = Some(b.clone());
                    return ResolveResult::Success(());
                }
            }
            (a, &Type::InferenceVariable(key)) => {
                let infer_info = self.checker.query_inference_variable_mut(key);
                if let Some(typ) = &infer_info.ty {
                    let ty = typ.clone();
                    return self.equate_types(a, &ty, loc);
                } else {
                    if let &Type::InferenceVariable(a_key) = a {
                        if a_key == key {
                            return ResolveResult::Success(());
                        }
                    }
                    infer_info.ty = Some(a.clone());
                    return ResolveResult::Success(());
                }
            }
            _ => {
                todo!()
            }
        }
    }

    // fn coerce_to_struct_ref(&mut self, expr: AnnotatedExpr<'a>) -> AnnotatedExpr<'a> {
    //     let AnnotatedExpr { expr, ty, always_diverges } = expr;
    // }

    fn coerce_to_type(&mut self, expr: AnnotatedExpr<'a>, to_ty: &Type) -> AnnotatedExpr<'a> {
        let AnnotatedExpr { expr, ty, always_diverges } = expr;

        let failure = |s: &Self| tc::TypeCheckError::IncompatibleTypes { expected: s.display_type(to_ty), got: s.display_type(&ty), loc: expr.loc() };

        match (&ty, to_ty) {
            (Type::Errored, _) | (_, Type::Errored) => {
                return AnnotatedExpr::new_errored(expr.loc(), always_diverges);
            },
            (Type::Never, to_ty) => {
                return AnnotatedExpr { expr: hir::Expr::CoerceFromNever(Box::new(expr), to_ty.clone()), ty: to_ty.clone(), always_diverges };
            },
            (Type::Boolean, Type::Boolean) => return AnnotatedExpr { expr, ty, always_diverges },
            (Type::Unit, Type::Unit) => return AnnotatedExpr { expr, ty, always_diverges },
            (&Type::SignedInteger(from_bits), &Type::SignedInteger(to_bits)) => {
                if from_bits == to_bits {
                    return AnnotatedExpr { expr, ty, always_diverges }
                } else if from_bits < to_bits {
                    return AnnotatedExpr { expr: hir::Expr::SignExtend(Box::new(expr), to_bits), ty: Type::SignedInteger(to_bits), always_diverges }
                } else {
                    todo!()
                }
            },
            (Type::TypeParameter(a_key), Type::TypeParameter(b_key)) => {
                if a_key == b_key {
                    return AnnotatedExpr { expr, ty, always_diverges };
                } else {
                    self.push_error(failure(self));
                    return AnnotatedExpr::new_errored(expr.loc(), false);
                }
            }
            (Type::Struct(StructType(from_struct, from_variant)), Type::Struct(StructType(to_struct, to_variant))) => {
                if from_struct == to_struct {
                    assert_eq!(from_variant.len(), to_variant.len());

                    for (from, to) in from_variant.iter().zip(to_variant) {
                        if self.equate_types(from, to, expr.loc()).is_failure() {
                            self.push_error(failure(self));
                            return AnnotatedExpr::new_errored(expr.loc(), false);
                        }
                    }
                    return AnnotatedExpr { expr, ty, always_diverges };
                } else {
                    self.push_error(failure(self));
                    return AnnotatedExpr::new_errored(expr.loc(), false);
                }
            },
            (Type::Function(FunctionType(a_params, a_ret)), Type::Function(FunctionType(b_params, b_ret))) => {
                let mut failed = false;
                for (from, to) in a_params.iter().zip(b_params) {
                    if self.equate_types(from, to, expr.loc()).is_failure() {
                        failed = true;
                    }
                }
                if self.equate_types(a_ret, b_ret, expr.loc()).is_failure() {
                    failed = true;
                }
                if a_params.len() != b_params.len() {
                    failed = true;
                }
                if failed {
                    self.push_error(failure(self));
                    return AnnotatedExpr::new_errored(expr.loc(), always_diverges);
                } else {
                    return AnnotatedExpr { expr, ty, always_diverges };
                }
            },
            (&Type::InferenceVariable(key), to_type) => {
                let infer_info = self.checker.query_inference_variable_mut(key);
                if let Some(typ) = &infer_info.ty {
                    let ty = typ.clone();
                    return self.coerce_to_type(AnnotatedExpr { expr, ty, always_diverges }, to_type);
                } else {
                    infer_info.ty = Some(to_type.clone());
                    return AnnotatedExpr { expr, ty, always_diverges };
                }
            },
            (from_typ, &Type::InferenceVariable(key)) => {
                let infer_info = self.checker.query_inference_variable_mut(key);
                if let Some(typ) = &infer_info.ty {
                    let typ = typ.clone();
                    return self.coerce_to_type(AnnotatedExpr { expr, ty, always_diverges }, &typ)
                } else {
                    infer_info.ty = Some(from_typ.clone());
                    return AnnotatedExpr { expr, ty, always_diverges };
                }
            },
            (_, _) => {
                self.push_error(failure(self));
                return AnnotatedExpr::new_errored(expr.loc(), always_diverges);
            }
        };
    }

    fn resolve_block(&mut self, block: &ast::Block<'a>, yield_type: ExpectedType, block_ns: tc::NamespaceKey) -> hir::Block<'a> {
        let ast::Block { stmts, trailing_expr, loc } = block;

        let (always_diverges, stmts, trailing_expr) = {
            let mut child = ResolveContext {
                checker: self.checker,
                types: self.types,
                prototypes: self.prototypes,
                namespace: block_ns,
                expected_return: self.expected_return.clone(),
                level: self.level,
                return_types: vec![]
            };
            let mut hir_stmts: Vec<hir::Stmt> = Vec::new();
            let mut diverges = false;
            for stmt in stmts {
                let AnnotatedStmt { stmt, always_diverges } = child.resolve_stmt(stmt);
                hir_stmts.push(stmt);
                diverges |= always_diverges;
            }

            let trailing_expr = match trailing_expr {
                Some(expr) => {
                    let expr = child.resolve_expr(expr, yield_type);
                    diverges |= expr.always_diverges;
                    expr
                },
                None => {
                    let trailing = if diverges {
                        AnnotatedExpr { expr: hir::Expr::Never(block.loc), ty: Type::Never, always_diverges: true }
                    } else {
                        AnnotatedExpr { expr: hir::Expr::Unit(block.loc), ty: Type::Unit, always_diverges: false }
                    };
                    if let Some(yield_type) = yield_type {
                        self.coerce_to_type(trailing, &yield_type)
                    } else {
                        trailing
                    }
                }
            };

            (diverges, hir_stmts, trailing_expr)
        };

        let declared = self.checker.namespaces[block_ns].get_names();
        hir::Block { stmts, trailing_expr: Box::new(trailing_expr.expr), yield_type: trailing_expr.ty, always_diverges, declared, loc: *loc }
    }

    fn resolve_stmt(&mut self, stmt: &'b ast::Stmt<'a>) -> AnnotatedStmt<'a> {
        match stmt {
            ast::Stmt::Decl { name, typ, value, loc } => {
                let expected_type = match typ {
                    Some(t) => Some(self.resolve_type(t)),
                    None => None
                };
                let AnnotatedExpr { expr, ty, always_diverges } = self.resolve_expr(value, expected_type);
                let (decl, _) = self.add_local(name.clone(), ty, *loc);
                // todo error if previously defined
                AnnotatedStmt { stmt: hir::Stmt::Decl { decl, value: expr, loc: *loc }, always_diverges }
            },
            ast::Stmt::Expr { expr, loc } => {
                let AnnotatedExpr { expr, always_diverges, .. } = self.resolve_expr(expr, None);
                AnnotatedStmt { stmt: hir::Stmt::Expr { expr, loc: *loc }, always_diverges }
            },
            ast::Stmt::Return { value, loc} => {
                let expected_return = self.expected_return.clone();
                let AnnotatedExpr { expr, ty, .. } = self.resolve_expr(value, expected_return);

                self.return_types.push(ty);
                AnnotatedStmt { stmt: hir::Stmt::Return { value: expr, loc: *loc }, always_diverges: true }
            }
        }
    }

    // fn resolve_method(&mut self, )

    fn resolve_expr(&mut self, expr: &'b ast::Expr<'a>, yield_type: ExpectedType) -> AnnotatedExpr<'a> {
        let resolved_expr = match expr {
            ast::Expr::Integer { number, loc } => {
                AnnotatedExpr { expr: hir::Expr::Integer(*number, *loc), ty: Type::SignedInteger(32), always_diverges: false }
            }
            ast::Expr::Bool { value, loc} => {
                AnnotatedExpr { expr: hir::Expr::Bool(*value, *loc), ty: Type::Boolean, always_diverges: false }
            }
            ast::Expr::Name { name, loc } => {
                let resolved = self.resolve_name(name);
                let decl = match resolved {
                    Some(key) => key,
                    None => {
                        self.push_error(tc::TypeCheckError::CouldNotResolveName(name.clone(), *loc));
                        return AnnotatedExpr::new_errored(*loc, false);
                    }
                };
                match &self.checker.names[decl] {
                    hir::NameInfo::Local { ty, .. } => {
                        AnnotatedExpr { expr: hir::Expr::Name(decl, *loc), ty: ty.clone(), always_diverges: false }
                    }
                    hir::NameInfo::Function { key, .. } => {
                        if let Some(fn_ty) = self.prototypes.functions[*key].typ() {
                            AnnotatedExpr { expr: hir::Expr::Name(decl, *loc), ty: fn_ty.into(), always_diverges: false }
                        } else {

                            return AnnotatedExpr::new_errored(*loc, false);
                        }
                    }
                }
            },
            ast::Expr::Block(block) => {
                let block_ns = self.checker.add_namespace(Some(self.namespace));
                let block = self.resolve_block(block, yield_type.clone(), block_ns);
                AnnotatedExpr { ty: block.yield_type.clone(), always_diverges: block.always_diverges, expr: hir::Expr::Block(block) }
            }
            ast::Expr::Call { callee, arguments, loc } => {
                match callee.as_ref() {
                    ast::Expr::Name { name, loc: name_loc } => {
                        let Some(name_key) = self.resolve_name(name) else {
                            self.push_error(tc::TypeCheckError::CouldNotResolveName(name.clone(), *name_loc));
                            return AnnotatedExpr::new_errored(*name_loc, false);
                        };
                        match self.checker.names[name_key] {
                            hir::NameInfo::Function { key, .. } => {
                                self.resolve_generic_call(key, arguments, &yield_type, *loc)
                            }
                            _ => {
                                self.resolve_call(callee, arguments, *loc)
                            }
                        }
                    }
                    _ => {
                        self.resolve_call(callee, arguments, *loc)
                    }
                }
            }
            ast::Expr::CreateStruct { struct_, arguments, loc, .. } => {
                let mut diverges = false;
                let Some(struct_key) = self.checker.resolve_struct(struct_, self.namespace) else {
                    self.push_error(tc::TypeCheckError::ExpectedStructName { got: struct_.clone(), loc: *loc });
                    return AnnotatedExpr::new_errored(*loc, false);
                };

                let mut map = HashMap::new();
                let mut inference_variables = Vec::new();
                for (_, type_param) in &self.types.structs[struct_key].type_parameters {
                    let name = self.checker.type_parameters[*type_param].name.clone();
                    let inference_key = self.checker.add_inference_variable(name, *loc);
                    inference_variables.push((*type_param, inference_key));
                    map.insert(*type_param, Type::InferenceVariable(inference_key));
                }

                let mut expected_fields = self.prototypes.structs[struct_key].fields.clone();
                let mut resolved_fields = IndexMap::new();
                let mut extraneous_fields_provided = false;
                for argument in arguments {
                    let name = argument.name.clone();
                    if let Some(field_info) = expected_fields.remove(&name) {
                        let expected_type = Some(self.checker.subs(&field_info.ty, &map));
                        let resolved = self.resolve_expr(&argument.argument, expected_type);
                        diverges |= resolved.always_diverges;
                        resolved_fields.insert(name, Box::new(resolved.expr));
                    } else {
                        self.push_error(tc::TypeCheckError::NoSuchFieldName { field: name.clone(), typ: struct_.clone(), loc: argument.name_loc });
                        extraneous_fields_provided = true;
                    }
                };
                if !expected_fields.is_empty() {
                    self.push_error(tc::TypeCheckError::MissingFields { fields: expected_fields.into_iter().map(|(name, _)| name).collect(), typ: struct_.clone(), loc: *loc });
                    return AnnotatedExpr::new_errored(*loc, false);
                }
                if extraneous_fields_provided {
                    return AnnotatedExpr::new_errored(*loc, false);
                }

                let mut variant = vec![];
                for (type_param_key, inference_variable) in inference_variables {
                    let info = self.checker.query_inference_variable_mut(inference_variable);
                    if let Some(typ) = &info.ty {
                        variant.push(typ.clone());
                    } else {
                        let name = self.checker.type_parameters[type_param_key].name.clone();
                        self.push_error(tc::TypeCheckError::CouldNotInferTypeParameter(name, *loc));
                    }
                }
                if variant.len() != map.len() {
                    return AnnotatedExpr::new_errored(*loc, false);
                }

                let struct_type = StructType(struct_key, variant);

                AnnotatedExpr {
                    ty: struct_type.clone().into(),
                    expr: hir::Expr::CreateStruct { struct_type, fields: resolved_fields, loc: *loc },
                    always_diverges: diverges
                }
            }
            ast::Expr::New { value, loc } => {
                let resolved_value = match &yield_type {
                    Some(Type::Gc(inner)) => {
                        self.resolve_expr(value, Some(inner.as_ref().clone()))
                    }
                    Some(_) => {
                        self.resolve_expr(value, None)
                    }
                    None => {
                        self.resolve_expr(value, None)
                    }
                };

                AnnotatedExpr {
                    ty: Type::Gc(Box::new(resolved_value.ty)),
                    expr: hir::Expr::New { value: Box::new(resolved_value.expr), loc: *loc },
                    always_diverges: resolved_value.always_diverges
                }
            }
            ast::Expr::GetAttr { obj, attr, loc } => {
                let AnnotatedExpr { expr, ty, mut always_diverges } = self.resolve_expr(obj, None);

                let mut object_ty = ty;
                let mut coercions = Vec::new();
                let struct_type = loop {
                    match object_ty {
                        Type::Struct(struct_type) => break struct_type,
                        Type::Gc(inner) => {
                            coercions.push(Coercion::DerefGc);
                            object_ty = *inner;
                        }
                        Type::Errored => {
                            return AnnotatedExpr::new_errored(*loc, false);
                        }
                        ty => {
                            self.push_error(tc::TypeCheckError::ExpectedStruct { got: self.display_type(&ty), loc: *loc });
                            return AnnotatedExpr::new_errored(*loc, false);
                        }
                    }
                };

                let map = self.types.create_subs(struct_type.clone());

                let Some(field_info) = self.prototypes.structs[struct_type.0].fields.get(attr) else {
                    self.push_error(tc::TypeCheckError::NoSuchFieldName { typ: self.types.structs[struct_type.0].name.clone(), field: attr.clone(), loc: *loc });
                    return AnnotatedExpr::new_errored(*loc, false);
                };
                let field_ty = self.checker.subs(&field_info.ty, &map);

                AnnotatedExpr {
                    expr: hir::Expr::GetAttr { obj: Box::new(expr), coercions, obj_type: struct_type, field_ty: field_ty.clone(), attr: attr.clone(), loc: *loc },
                    ty: field_ty,
                    always_diverges
                }
            },
            ast::Expr::MethodCall { object, method, arguments, loc } => {
                let AnnotatedExpr { expr, ty, mut always_diverges } = self.resolve_expr(object, None);

                let mut object_type = ty;
                let mut coercions = Vec::new();
                let (method_key, type_map) = loop {
                    let mut possible_methods = self.prototypes.get_method(&object_type, method);
                    if possible_methods.is_empty() {
                        match object_type {
                            Type::Gc(inner) => {
                                coercions.push(Coercion::DerefGc);
                                object_type = *inner;
                            }
                            Type::Errored => return AnnotatedExpr::new_errored(*loc, always_diverges),
                            other => {
                                self.push_error(tc::TypeCheckError::NoSuchMethodName { on_type: self.display_type(&other), method: method.clone(), loc: *loc });
                                return AnnotatedExpr::new_errored(*loc, false);
                            }
                        }
                    } else if possible_methods.len() > 1 {
                        self.push_error(tc::TypeCheckError::ConflictingMethods {
                            on_type: self.display_type(&object_type), method: method.clone(), loc: *loc,
                            possible: possible_methods.into_iter().map(|(key, _)| self.prototypes.methods[key].ast_method.loc).collect()
                        });
                        return AnnotatedExpr::new_errored(*loc, false);
                    } else {
                        break possible_methods.remove(0);
                    }
                };
                let method_info = &self.prototypes.methods[method_key];

                let mut resolved_arguments = Vec::new();
                for (param, arg) in method_info.params.values().zip(arguments) {
                    let resolved_arg = self.resolve_expr(arg, Some(self.checker.subs(&param.ty, &type_map)));
                    always_diverges |= resolved_arg.always_diverges;
                    resolved_arguments.push(resolved_arg.expr);
                }
                let ret = self.checker.subs(&method_info.ret, &type_map);

                if method_info.params.len() != arguments.len() {
                    self.push_error(tc::TypeCheckError::MismatchedArguments { expected: method_info.params.len(), got: arguments.len(), loc: *loc });
                    return AnnotatedExpr::new_errored(*loc, always_diverges);
                }

                AnnotatedExpr {
                    expr: hir::Expr::MethodCall {
                        object: Box::new(expr), coercions, method: method_key, arguments: resolved_arguments, loc: *loc
                    },
                    ty: ret,
                    always_diverges
                }
            }
            ast::Expr::GenericCall { .. } => todo!(),
            ast::Expr::BinOp { .. } => todo!(),
            ast::Expr::IfElse { cond, then_do, else_do, loc } => {
                let resolved_cond = self.resolve_expr(cond, Some(Type::Boolean));
                let mut diverges = resolved_cond.always_diverges;

                let (then_do, else_do, yield_type) = if let Some(expected_type) = &yield_type {
                    let resolved_then_do = self.resolve_expr(then_do, Some(expected_type.clone()));
                    let resolved_else_do = self.resolve_expr(else_do, Some(expected_type.clone()));
                    diverges |= resolved_then_do.always_diverges | resolved_else_do.always_diverges;
                    (resolved_then_do.expr, resolved_else_do.expr, expected_type.clone())
                } else {
                    let resolved_then_do = self.resolve_expr(then_do, None);
                    diverges |= resolved_then_do.always_diverges;
                    match resolved_then_do.ty.clone() {
                        Type::Never => {
                            let resolved_else_do = self.resolve_expr(else_do, None);
                            let expr_yield_type = resolved_else_do.ty;
                            let then_do = self.coerce_to_type(resolved_then_do, &expr_yield_type);
                            diverges |= resolved_else_do.always_diverges;
                            (then_do.expr, resolved_else_do.expr, expr_yield_type)
                        },
                        ty => {
                            let resolved_else_do = self.resolve_expr(else_do, Some(ty.clone()));
                            diverges |= resolved_else_do.always_diverges;
                            (resolved_then_do.expr, resolved_else_do.expr, ty)
                        },
                    }
                };

                AnnotatedExpr {
                    expr: hir::Expr::IfElse { cond: Box::new(resolved_cond.expr), then_do: Box::new(then_do), else_do: Box::new(else_do), yield_type: yield_type.clone(), loc: *loc },
                    ty: yield_type,
                    always_diverges: diverges
                }
            }
            ast::Expr::Closure { parameters, body, loc } => {
                let closure_ns = self.checker.add_namespace(Some(self.namespace));

                let expected_return: ExpectedType;
                let mut parameter_types: Vec<Type> = vec![];

                if let Some(ty) = yield_type.clone() {
                    let Type::Function(FunctionType(params, ret)) = &ty else {
                        self.push_error(tc::TypeCheckError::UnexpectedClosure { expected: self.display_type(&ty), loc: *loc });
                        return AnnotatedExpr::new_errored(*loc, false);
                    };
                    expected_return = Some(ret.as_ref().clone());

                    if parameters.len() != params.len() {
                        self.push_error(tc::TypeCheckError::ClosureWithWrongParameters { expected: self.display_type(&ty), got: parameters.len(), loc: *loc});
                        return AnnotatedExpr::new_errored(*loc, false);
                    }

                    let mut failed = false;

                    for (param, expected_type) in parameters.iter().zip(params) {
                        if let Some(ty) = &param.typ {
                            let ty = self.resolve_type(ty);
                            if self.equate_types(&ty, expected_type, *loc).ok(self.checker).is_none() {
                                failed = true;
                            }
                        }
                        parameter_types.push(expected_type.clone());
                    }

                    if failed {
                        return AnnotatedExpr::new_errored(*loc, false);
                    }
                } else {
                    for param in parameters {
                        if let Some(ty) = &param.typ {
                            parameter_types.push(self.resolve_type(ty));
                        } else {
                            self.push_error(tc::TypeCheckError::CouldNotInferParameters(*loc));
                            return AnnotatedExpr::new_errored(*loc, false);
                        }
                    }
                    expected_return = None;
                }

                let mut child = ResolveContext {
                    namespace: closure_ns,
                    expected_return: expected_return.clone(),
                    checker: self.checker,
                    types: self.types,
                    level: self.level + 1,
                    return_types: vec![],
                    prototypes: self.prototypes,
                };

                let mut resolved_parameters = vec![];
                for (parameter, parameter_type) in parameters.iter().zip(&parameter_types) {
                    let (key, _) = child.add_local(parameter.name.clone(), parameter_type.clone(), parameter.loc);
                    // todo error if previously defined
                    resolved_parameters.push(hir::Parameter { name: parameter.name.clone(), typ: parameter_type.clone(), decl: key, loc: parameter.loc });
                }

                let resolved_body = child.resolve_expr(body, expected_return.clone());

                let yield_type = if let Some(return_type) = expected_return {
                    return_type
                } else {
                    if !child.return_types.is_empty() {
                        panic!();
                    }
                    resolved_body.ty
                };
                let fn_type = FunctionType(parameter_types, Box::new(yield_type.clone()));

                let declared = self.checker.namespaces[closure_ns].get_names();

                let body = hir::Block { stmts: vec![], trailing_expr: Box::new(resolved_body.expr), always_diverges: resolved_body.always_diverges, yield_type: yield_type.clone(), declared, loc: body.loc() };

                AnnotatedExpr {
                    expr: hir::Expr::Closure { parameters: resolved_parameters, body, fn_type: fn_type.clone(), loc: *loc },
                    ty: fn_type.into(),
                    always_diverges: false
                }
            }
        };

        if let Some(expected_ty) = yield_type {
            self.coerce_to_type(resolved_expr, &expected_ty)
        } else {
            resolved_expr
        }
    }

    fn resolve_call(&mut self, callee: &'b ast::Expr<'a>, arguments: &'b Vec<ast::Expr<'a>>, loc: Location<'a>) -> AnnotatedExpr<'a> {
        let resolved_callee = self.resolve_expr(callee, None);
        let mut diverges = resolved_callee.always_diverges;

        match &resolved_callee.ty {
            Type::Function(callee_ty @ FunctionType(params, ret)) => {
                let mut resolved_arguments = Vec::new();
                for (param, arg) in params.iter().zip(arguments) {
                    let resolved_arg = self.resolve_expr(arg, Some(param.clone()));
                    diverges |= resolved_arg.always_diverges;
                    resolved_arguments.push(resolved_arg.expr);
                }

                if params.len() != arguments.len() {
                    self.push_error(tc::TypeCheckError::MismatchedArguments { expected: params.len(), got: arguments.len(), loc });
                    return AnnotatedExpr::new_errored(loc, diverges);
                }

                AnnotatedExpr {
                    expr: hir::Expr::Call { callee: Box::new(resolved_callee.expr), callee_type: callee_ty.clone(), arguments: resolved_arguments, loc },
                    ty: ret.as_ref().clone(),
                    always_diverges: diverges
                }
            }
            Type::Errored => {
                return AnnotatedExpr::new_errored(loc, diverges);
            }
            _ => {
                self.push_error(tc::TypeCheckError::ExpectedFunction { got: self.display_type(&resolved_callee.ty), loc: callee.loc() });
                return AnnotatedExpr::new_errored(loc, diverges);
            }
        }
    }

    fn resolve_generic_call(&mut self, func: hir::FunctionKey, arguments: &'b Vec<ast::Expr<'a>>, yield_type: &ExpectedType, loc: Location<'a>) -> AnnotatedExpr<'a> {
        let info = &self.prototypes.functions[func];

        let mut diverges = false;

        let mut map = HashMap::new();
        let mut inference_variables: Vec<InferenceVariableKey> = Vec::new();
        for type_param in info.type_parameters.values() {
            let name = self.checker.type_parameters[*type_param].name.clone();
            let inference_key = self.checker.add_inference_variable(name, loc);
            map.insert(*type_param, Type::InferenceVariable(inference_key));
            inference_variables.push(inference_key);
        }

        let ret = self.checker.subs(&info.ret, &map);
        if let Some(ty) = yield_type {
            let _ = self.equate_types(&ret, ty, loc);
        }

        let mut resolved_arguments = Vec::new();
        for (param, arg) in info.params.values().zip(arguments.iter()) {
            let resolved_arg = self.resolve_expr(arg, Some(self.checker.subs(&param.ty, &map)));
            diverges |= resolved_arg.always_diverges;
            resolved_arguments.push(resolved_arg.expr);
        }

        if info.params.len() != arguments.len() {
            self.push_error(tc::TypeCheckError::MismatchedArguments { expected: info.params.len(), got: arguments.len(), loc });
            return AnnotatedExpr::new_errored(loc, diverges);
        }

        let mut generic_tuple = vec![];
        for (type_param, inference_variable) in info.type_parameters.values().zip(inference_variables) {
            let info = self.checker.query_inference_variable_mut(inference_variable);
            if let Some(ty) = &info.ty {
                generic_tuple.push(ty.clone());
            } else {
                let name = self.checker.type_parameters[*type_param].name.clone();
                self.push_error(tc::TypeCheckError::CouldNotInferTypeParameter(name, loc));
            }
        };
        if generic_tuple.len() != map.len() {
            return AnnotatedExpr::new_errored(loc, diverges);
        }

        AnnotatedExpr {
            expr: hir::Expr::GenericCall { generic: generic_tuple, callee: func, arguments: resolved_arguments, ret_type: ret.clone(), loc },
            ty: ret,
            always_diverges: diverges
        }
    }
}
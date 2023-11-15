use std::collections::HashMap;
use indexmap::IndexMap;
use slotmap::SlotMap;
use crate::parsing::ast;
use crate::lowering::type_check::{TypeCheck, NamespaceKey, TypeCheckError, resolve_type, DisplayType, resolve_struct};
use crate::lowering::hir::*;
use crate::lowering::type_check::collect_functions::CollectedFunctions;
use crate::source::HasLoc;


pub(super) fn collect_function_bodies(collected: CollectedFunctions) -> Result<HIR, Vec<TypeCheckError>> {
    let CollectedFunctions { mut checker, files, file_namespaces, function_namespaces, .. } = collected;

    for (file_path, file) in &files {
        let file_ns = file_namespaces[file_path];

        for ast_func in file.iter_functions() {
            let func_key = checker.get_function_key(file_ns, &ast_func.name);
            let func_ns = function_namespaces[func_key];

            let func = &checker.hir.function_prototypes[func_key];
            let ret = func.ret.clone();

            let mut core = ResolveContextCore::new();
            let mut resolver = ResolveContext::create_for_function(&mut checker, Some(ret.clone()), func_ns, &mut core);

            let body = resolver.resolve_block(&ast_func.body, Some(ret), Some(func_ns));
            checker.hir.function_bodies.insert(func_key, FunctionBody { body });
        }
    }

    checker.finalize()
}


type ExpectedType = Option<Type>;

struct ResolveContextCore {
    generic_stack: SlotMap<TypeParamKey, Option<Type>>
}

impl ResolveContextCore {
    fn new() -> ResolveContextCore {
        ResolveContextCore {
            generic_stack: SlotMap::with_key()
        }
    }
}

struct ResolveContext<'a, 'b> where 'a: 'b {
    checker: &'b mut TypeCheck<'a>,
    expected_return: Option<Type>,
    namespace: NamespaceKey,
    core: &'b mut ResolveContextCore,
    level: usize,

    return_types: Vec<Type>
}

impl<'a, 'b> ResolveContext<'a, 'b>  where 'a: 'b  {
    fn create_for_function(checker: &'b mut TypeCheck<'a>, expected_return: Option<Type>, namespace: NamespaceKey, core: &'b mut ResolveContextCore) -> ResolveContext<'a, 'b> {
        ResolveContext {
            checker, expected_return, namespace, core,
            level: 0,
            return_types: vec![]
        }
    }

    fn push_error(&self, error: TypeCheckError<'a>) {
        self.checker.push_error(error)
    }

    fn resolve_name(&self, name: &str) -> Option<NameKey> {
        self._resolve_name(self.namespace, name)
    }

    fn _resolve_name(&self, ns: NamespaceKey, name: &str) -> Option<NameKey> {
        match self.checker.namespaces[ns].names.get(name) {
            Some(n) => Some(*n),
            None => self.checker.namespaces[ns].parent.and_then(|p| self._resolve_name(p, name))
        }
    }

    fn add_name(&mut self, name: String, info: NameInfo<'a>) -> NameKey {
        self.checker.add_name(self.namespace, name, info)
    }

    fn resolve_type(&mut self, typ: &ast::Type<'a>) -> Type {
        resolve_type(self.checker, self.namespace, typ)
    }

    fn type_of(&self, expr: &Expr<'a>) -> Type {
        self.checker.type_of_expr(expr)
    }

    fn display_type(&self, ty: &Type) -> DisplayType<'a> {
        match ty {
            Type::Unit => DisplayType::Unit,
            Type::Never => DisplayType::Never,
            Type::Boolean => DisplayType::Boolean,
            Type::Errored => DisplayType::Errored,
            Type::Integer { bits } => DisplayType::Integer { bits: *bits },
            Type::Struct { struct_, variant } => {
                let s = &self.checker.hir.structs[*struct_];
                DisplayType::Struct {
                    name: s.name.clone(),
                    variant: variant.iter().map(|t| self.display_type(t)).collect(),
                    loc: s.loc
                }
            },
            Type::Function { params, ret } => DisplayType::Function {
                params: params.iter().map(|t| self.display_type(t)).collect(),
                ret: Box::new(self.display_type(ret))
            },
            Type::GenericFunction { type_params, params, ret, .. } => {
                let type_params: Vec<_> = type_params.iter().map(|p| self.display_type(&p.as_type())).collect();
                let params: Vec<_> = params.iter().map(|p| self.display_type(p)).collect();
                let ret = Box::new(self.display_type(ret));
                DisplayType::GenericFunction { type_params, params, ret }
            },
            Type::TypeParameter { name, bound, .. } => {
                DisplayType::TypeParameter { name: name.clone(), bound: bound.as_ref().map(|t| Box::new(self.display_type(t))) }
            }
            Type::TypeParameterInstance { name, id, .. } => {
                if let Some(ty) = &self.core.generic_stack[*id] {
                    self.display_type(ty)
                } else {
                    DisplayType::TypeParamInstance { name: name.clone() }
                }
            }
        }
    }

    fn cast_to_maybe_type(&mut self, to_type: Option<Type>, resolved_expr: Expr<'a>) -> Expr<'a> {
        if let Some(ty) = to_type {
            self.cast_to_type(resolved_expr, &ty)
        } else {
            resolved_expr
        }
    }

    fn cast_to_type(&mut self, resolved_expr: Expr<'a>, to_type: &Type) -> Expr<'a> {
        let actual_type = self.type_of(&resolved_expr);

        let incompatible_types = || {
            self.push_error(TypeCheckError::IncompatibleTypes {
                expected: self.display_type(to_type),
                got: self.display_type(&actual_type),
                loc: resolved_expr.loc()
            });
            Expr::Errored { loc: resolved_expr.loc() }
        };
        if &actual_type == to_type {
            resolved_expr
        } else {
            match (&actual_type, to_type) {
                (Type::Errored, _) | (_, Type::Errored) => {
                    return Expr::Errored { loc: resolved_expr.loc() };
                },
                (Type::Never, to_ty) => Expr::Cast(Box::new(resolved_expr), to_ty.clone()),
                (Type::Integer { bits: a }, Type::Integer { bits: b}) => {
                    if a <= b {
                        return Expr::Cast(Box::new(resolved_expr), Type::Integer { bits: *b });
                    } else {
                        return incompatible_types();
                    }
                },
                (Type::Struct { struct_: a, variant: a_var}, Type::Struct { struct_: b, variant: b_var}) => {
                    let mut curr_struct = (a, a_var);
                    while curr_struct != (b, b_var) {
                        if let Some((super_key, super_var, _)) = &self.checker.hir.structs[*curr_struct.0].super_struct {
                            curr_struct = (super_key, super_var);
                        } else {
                            return incompatible_types();
                        }
                    }

                    return Expr::Cast(Box::new(resolved_expr), Type::Struct { struct_: *curr_struct.0, variant: curr_struct.1.clone()});
                },
                (Type::Function { params: a_params, ret: a_ret }, Type::Function { params: b_params, ret: b_ret }) => {
                    todo!()
                },
                (Type::TypeParameterInstance { id, .. }, to_type) => {
                    let inferred_type = (&self.core.generic_stack[*id]).as_ref().cloned();
                    if let Some(typ) = inferred_type {
                        if self.can_cast_to_type(&typ, to_type) {
                            return Expr::Cast(Box::new(resolved_expr), to_type.clone());
                        } else {
                            self.push_error(TypeCheckError::IncompatibleTypes {
                                expected: self.display_type(to_type),
                                got: self.display_type(&actual_type),
                                loc: resolved_expr.loc()
                            });
                            return Expr::Errored { loc: resolved_expr.loc() };
                        }
                    } else {
                        self.core.generic_stack[*id] = Some(to_type.clone());
                        return resolved_expr;
                    }
                },
                (from_typ, Type::TypeParameterInstance { id, .. }) => {
                    let inferred_type = (&self.core.generic_stack[*id]).as_ref().cloned();
                    if let Some(typ) = inferred_type {
                        if self.can_cast_to_type(from_typ, &typ) {
                            return Expr::Cast(Box::new(resolved_expr), typ);
                        } else {
                            self.push_error(TypeCheckError::IncompatibleTypes {
                                expected: self.display_type(to_type),
                                got: self.display_type(&actual_type),
                                loc: resolved_expr.loc()
                            });
                            return Expr::Errored { loc: resolved_expr.loc() };
                        }
                    } else {
                        self.core.generic_stack[*id] = Some(from_typ.clone());
                        return resolved_expr;
                    }
                },
                (_, _) => {
                    return incompatible_types();
                }
            }
        }
    }

    fn can_cast_to_maybe_type(&mut self, from_type: &Type, to_type: Option<&Type>) -> bool {
        to_type.map_or(true, |ty| self.can_cast_to_type(from_type, ty))
    }

    fn can_cast_to_type(&mut self, from_type: &Type, to_type: &Type) -> bool {
        if from_type == to_type {
            true
        } else {
            match (from_type, to_type) {
                (Type::Errored, _) | (_, Type::Errored) => {
                    return true;
                },
                (Type::Never, _) => true,
                (Type::Integer { bits: a }, Type::Integer { bits: b}) => {
                    return a <= b;
                },
                (Type::Struct { struct_: a, variant: a_var}, Type::Struct { struct_: b, variant: b_var}) => {
                    let mut curr_struct = (a, a_var);
                    while curr_struct != (b, b_var) {
                        if let Some((super_key, super_var, _)) = &self.checker.hir.structs[*curr_struct.0].super_struct {
                            curr_struct = (super_key, super_var);
                        } else {
                            return false;
                        }
                    }

                    return true;
                },
                (Type::Function { params: a_params, ret: a_ret }, Type::Function { params: b_params, ret: b_ret }) => {
                    todo!()
                },
                (Type::TypeParameterInstance { id, .. }, to_type) => {
                    let inferred_type = (&self.core.generic_stack[*id]).as_ref().cloned();
                    if let Some(typ) = inferred_type {
                        if self.can_cast_to_type(&typ, to_type) {
                            return true;
                        } else {
                            return false;
                        }
                    } else {
                        self.core.generic_stack[*id] = Some(to_type.clone());
                        return true;
                    }
                },
                (from_typ, Type::TypeParameterInstance { id, .. }) => {
                    let inferred_type = (&self.core.generic_stack[*id]).as_ref().cloned();
                    if let Some(typ) = inferred_type {
                        if self.can_cast_to_type(from_typ, &typ) {
                            return true;
                        } else {
                            return false;
                        }
                    } else {
                        self.core.generic_stack[*id] = Some(from_typ.clone());
                        return true;
                    }
                },
                (_, _) => {
                    return false;
                }
            }
        }
    }

    fn resolve_block(&mut self, block: &ast::Block<'a>, yield_type: ExpectedType, with_ns: Option<NamespaceKey>) -> Block<'a> {
        let ast::Block { stmts, trailing_expr, loc } = block;

        let block_ns = with_ns.unwrap_or_else(||
            self.checker.add_namespace(Some(self.namespace))
        );
        let stmts = {
            let mut child = ResolveContext {
                namespace: block_ns,
                expected_return: self.expected_return.clone(),
                checker: self.checker,
                core: self.core,
                level: 0,
                return_types: vec![]
            };
            let stmts: Vec<_> = stmts.iter().map(|stmt| child.resolve_stmt(stmt)).collect();
            stmts
        };
        let always_breaks = stmts.iter().any(|stmt| stmt.always_diverges(&self.checker.hir));

        let trailing_expr = match trailing_expr {
            Some(e) => {
                let mut child = ResolveContext {
                    namespace: block_ns,
                    expected_return: self.expected_return.clone(),
                    checker: self.checker,
                    core: self.core,
                    level: 0,
                    return_types: vec![]
                };
                Some(Box::new(child.resolve_expr(e, yield_type)))
            },
            None => {
                if always_breaks {
                    None
                } else {
                    if yield_type.is_none() || yield_type.is_some_and(|ty| matches!(ty, Type::Unit)) {
                        Some(Box::new(Expr::Unit { loc: *loc }))
                    } else {
                        Some(Box::new(Expr::Errored { loc: *loc }))
                    }
                }
            }
        };
        let declared = self.checker.namespaces[block_ns].get_names();
        let yield_type = if always_breaks {
            Type::Never
        } else if let Some(expr) = &trailing_expr {
            self.type_of(expr)
        } else {
            Type::Unit
        };
        Block { stmts, trailing_expr, yield_type, declared, loc: *loc }
    }

    fn is_never(&self, expr: &Expr<'a>) -> bool {
        if self.checker.hir.type_of_expr(expr).is_never(&self.checker.hir) {
            self.push_error(TypeCheckError::CannotUseNever { loc: expr.loc() });
            return true;
        } else {
            return false;
        }
    }

    fn resolve_expr(&mut self, expr: &ast::Expr<'a>, yield_type: ExpectedType) -> Expr<'a> {
        let to_type = yield_type.clone();
        let resolved_expr = match expr {
            ast::Expr::Integer { number, loc } => {
                Expr::Integer { num: *number, loc: *loc }
            }
            ast::Expr::Bool { value, loc} => {
                Expr::Bool { value: *value, loc: *loc }
            }
            ast::Expr::Name { name, loc } => {
                let resolved = self.resolve_name(name);
                let decl = match resolved {
                    Some(key) => key,
                    None => {
                        self.push_error(TypeCheckError::CouldNotResolveName(name.clone(), *loc));
                        return Expr::Errored { loc: *loc };
                    }
                };
                Expr::Name { decl, loc: *loc }
            },
            ast::Expr::Block(block) => {
                Expr::Block(self.resolve_block(block, yield_type, None))
            }
            ast::Expr::Call { callee, arguments, loc } => {
                let resolved_callee = self.resolve_expr(callee, None);
                if self.is_never(&resolved_callee) {
                    return Expr::Errored { loc: *loc };
                };

                let callee_ty = self.type_of(&resolved_callee);
                match &callee_ty {
                    Type::Function { params, .. } => {
                        if params.len() != arguments.len() {
                            self.push_error(TypeCheckError::MismatchedArguments { expected: params.len(), got: arguments.len(), loc: *loc });
                            return Expr::Errored { loc: *loc };
                        }
                        let mut resolved_arguments = Vec::new();
                        for (param, arg) in params.iter().zip(arguments.iter()) {
                            let resolved_arg = self.resolve_expr(arg, Some(param.clone()));
                            if self.is_never(&resolved_arg) {
                                return Expr::Errored { loc: *loc };
                            };
                            resolved_arguments.push(resolved_arg);
                        }
                        Expr::Call { callee: Box::new(resolved_callee), arguments: resolved_arguments, loc: *loc }
                    }
                    Type::GenericFunction { func, type_params, params, ret } => {
                        let mut map = HashMap::new();
                        for type_param in type_params {
                            let type_param_key = self.core.generic_stack.insert(None);
                            map.insert(type_param.id, Type::TypeParameterInstance { name: type_param.name.clone(), id: type_param_key });
                        }

                        if params.len() != arguments.len() {
                            self.push_error(TypeCheckError::MismatchedArguments { expected: params.len(), got: arguments.len(), loc: *loc });
                            return Expr::Errored { loc: *loc };
                        }

                        let mut resolved_arguments = Vec::new();
                        for (param, arg) in params.iter().zip(arguments.iter()) {
                            let resolved_arg = self.resolve_expr(arg, Some(param.subs(&map)));
                            if self.is_never(&resolved_callee) {
                                return Expr::Errored { loc: *loc };
                            };
                            resolved_arguments.push(resolved_arg);
                        }

                        if !self.can_cast_to_maybe_type(&ret.subs(&map), yield_type.as_ref()) {
                            self.push_error(TypeCheckError::IncompatibleTypes {
                                expected: self.display_type(yield_type.as_ref().unwrap()),
                                got: self.display_type(&ret.subs(&map)),
                                loc: *loc
                            });
                            return Expr::Errored { loc: *loc };
                        }

                        let mut generic_tuple = vec![];
                        for type_param in type_params {
                            let Type::TypeParameterInstance { id, .. } = map[&type_param.id] else {
                                panic!()
                            };
                            if let Some(typ) = &self.core.generic_stack[id] {
                                generic_tuple.push(typ.clone());
                            } else {
                                self.push_error(TypeCheckError::CouldNotInferTypeParameter(type_param.name.clone(), *loc));
                            }
                        };
                        if generic_tuple.len() != map.len() {
                            return Expr::Errored { loc: *loc };
                        }

                        Expr::GenericCall { generic: generic_tuple, callee: *func, arguments: resolved_arguments, loc: *loc }
                    }
                    Type::Errored => {
                        Expr::Errored { loc: *loc }
                    }
                    _ => {
                        self.push_error(TypeCheckError::ExpectedFunction { got: self.display_type(&callee_ty), loc: callee.loc() });
                        Expr::Errored { loc: *loc }
                    }
                }
            }
            ast::Expr::New { struct_, fields, loc, .. } => {
                let Some(struct_key) = resolve_struct(self.checker, self.namespace, struct_) else {
                    self.push_error(TypeCheckError::ExpectedStructName { got: struct_.clone(), loc: *loc });
                    return Expr::Errored { loc: *loc };
                };

                let mut map = HashMap::new();
                for type_param in &self.checker.hir.structs[struct_key].type_params {
                    let type_param_key = self.core.generic_stack.insert(None);
                    map.insert(type_param.id, Type::TypeParameterInstance { name: type_param.name.clone(), id: type_param_key });
                }

                let mut expected_fields = self.checker.hir.structs[struct_key].all_fields(&self.checker.hir);
                let mut resolved_fields = IndexMap::new();
                for field in fields {
                    let name = field.field_name.clone();
                    if !expected_fields.contains_key(&name) {
                        self.push_error(TypeCheckError::NoSuchFieldName { field: name.clone(), typ: struct_.clone(), loc: field.name_loc });
                        return Expr::Errored { loc: *loc };
                    }
                    let expected_type = expected_fields[&name].subs(&map);
                    let resolved = Box::new(self.resolve_expr(&field.argument, Some(expected_type)));
                    if self.is_never(&resolved) {
                        return Expr::Errored { loc: *loc };
                    };
                    expected_fields.remove(&name);
                    resolved_fields.insert(name, resolved);
                };
                if !expected_fields.is_empty() {
                    self.push_error(TypeCheckError::MissingFields { fields: expected_fields.into_iter().map(|p| p.0).collect(), typ: struct_.clone(), loc: *loc });
                    return Expr::Errored { loc: *loc };
                }

                let mut variant = vec![];
                for type_param in &self.checker.hir.structs[struct_key].type_params {
                    let Type::TypeParameterInstance { id, .. } = map[&type_param.id] else {
                        panic!()
                    };
                    if let Some(typ) = &self.core.generic_stack[id] {
                        variant.push(typ.clone());
                    } else {
                        self.push_error(TypeCheckError::CouldNotInferTypeParameter(type_param.name.clone(), *loc));
                    }
                }

                Expr::New { struct_: struct_key, variant, fields: resolved_fields, loc: *loc }
            }
            ast::Expr::GetAttr { obj, attr, loc } => {
                let resolved_obj = self.resolve_expr(obj, None);
                if self.is_never(&resolved_obj) {
                    return Expr::Errored { loc: *loc };
                };

                let ty = self.checker.hir.type_of_expr(&resolved_obj);

                let Type::Struct { struct_, variant } = ty else {
                    self.push_error(TypeCheckError::ExpectedStruct { got: self.display_type(&ty), loc: *loc });
                    return Expr::Errored { loc: *loc };
                };

                let mut map = HashMap::new();
                for (type_param, type_arg) in self.checker.hir.structs[struct_].type_params.iter().zip(variant.iter()) {
                    map.insert(type_param.id, type_arg.clone());
                }

                let Some(_) = self.checker.hir.structs[struct_].all_fields(&self.checker.hir).get(attr).cloned() else {
                    self.push_error(TypeCheckError::NoSuchFieldName { typ: self.checker.hir.structs[struct_].name.clone(), field: attr.clone(), loc: *loc });
                    return Expr::Errored { loc: *loc };
                };

                Expr::GetAttr { obj: Box::new(resolved_obj), attr: attr.clone(), loc: *loc }
            },
            ast::Expr::MethodCall { object, method, arguments, loc } => {
                let resolved_object = self.resolve_expr(object, None);
                if self.is_never(&resolved_object) {
                    return Expr::Errored { loc: *loc };
                }

                let Type::Struct { struct_, variant } = self.checker.hir.type_of_expr(&resolved_object) else {

                }
            }
            ast::Expr::GenericCall { .. } => todo!(),
            ast::Expr::BinOp { .. } => todo!(),
            ast::Expr::IfElse { cond, then_do, else_do, loc } => {
                let resolved_cond = Box::new(self.resolve_expr(cond, Some(Type::Boolean)));
                if self.is_never(&resolved_cond) {
                    return Expr::Errored { loc: *loc };
                };

                let resolved_then_do = Box::new(self.resolve_expr(then_do, yield_type.clone()));
                let resolved_else_do = Box::new(self.resolve_expr(else_do, yield_type.clone()));

                let expr_yield_type: Type;
                if resolved_then_do.always_diverges(&self.checker.hir) && resolved_else_do.always_diverges(&self.checker.hir) {
                    expr_yield_type = Type::Never;
                } else if let Some(yield_type) = yield_type {
                    expr_yield_type = yield_type;
                } else {
                    let then_ty = self.checker.hir.type_of_expr(&resolved_then_do);
                    let else_ty = self.checker.hir.type_of_expr(&resolved_else_do);

                    if then_ty.is_error() || else_ty.is_error() {
                        return Expr::Errored { loc: *loc };
                    }

                    if self.can_cast_to_type(&then_ty, &else_ty) {
                        expr_yield_type = else_ty;
                    } else if self.can_cast_to_type(&else_ty, &then_ty) {
                        expr_yield_type = then_ty;
                    } else {
                        self.push_error(TypeCheckError::IncompatibleTypes { expected: self.display_type(&then_ty), got: self.display_type(&else_ty), loc: else_do.loc() });
                        return Expr::Errored { loc: *loc };
                    }
                }

                Expr::IfElse { cond: resolved_cond, then_do: resolved_then_do, else_do: resolved_else_do, yield_type: expr_yield_type, loc: *loc }
            }
            ast::Expr::Closure { parameters, body, loc } => {
                let closure_ns = self.checker.add_namespace(Some(self.namespace));

                let expected_return;
                let mut parameter_types: Vec<Type> = vec![];

                if let Some(ty) = yield_type {
                    let Type::Function { params, ret } = &ty else {
                        self.push_error(TypeCheckError::UnexpectedClosure { expected: self.display_type(&ty), loc: *loc });
                        return Expr::Errored { loc: *loc };
                    };
                    expected_return = Some(ret.as_ref().clone());

                    if parameters.len() != params.len() {
                        self.push_error(TypeCheckError::ClosureWithWrongParameters { expected: self.display_type(&ty), got: parameters.len(), loc: *loc});
                        return Expr::Errored { loc: *loc };
                    }

                    for (param, expected_type) in parameters.iter().zip(params) {
                        if let Some(ty) = &param.typ {
                            parameter_types.push(self.resolve_type(ty));
                        } else {
                            parameter_types.push(expected_type.clone());
                        }
                    }
                } else {
                    for param in parameters {
                        if let Some(ty) = &param.typ {
                            parameter_types.push(self.resolve_type(ty));
                        } else {
                            self.push_error(TypeCheckError::CouldNotInferParameters(*loc));
                            return Expr::Errored { loc: *loc };
                        }
                    }
                    expected_return = None;
                }

                let mut child = ResolveContext {
                    namespace: closure_ns,
                    expected_return: expected_return.clone(),
                    checker: self.checker,
                    core: self.core,
                    level: self.level + 1,
                    return_types: vec![]
                };

                let mut resolved_parameters = vec![];
                for (parameter, parameter_type) in parameters.iter().zip(parameter_types) {
                    let key = child.add_name(parameter.name.clone(), NameInfo::Local { typ: parameter_type.clone(), loc: parameter.loc, level: child.level });
                    resolved_parameters.push(ClosureParameter { name: parameter.name.clone(), key, typ: parameter_type, loc: parameter.loc });
                }

                let resolved_body = child.resolve_expr(body, expected_return.clone());

                let ret_type: Type;
                let yield_type: Type;
                if let Some(return_type) = expected_return {
                    ret_type = return_type;
                    yield_type = self.type_of(&resolved_body);
                } else {
                    if !child.return_types.is_empty() {
                        panic!();
                    }
                    yield_type = self.type_of(&resolved_body);
                    ret_type = yield_type.clone();
                }

                let declared = self.checker.namespaces[closure_ns].get_names();
                let body = Block { stmts: vec![], trailing_expr: Some(Box::new(resolved_body)), yield_type, declared, loc: body.loc() };

                Expr::Closure { parameters: resolved_parameters, body, ret_type, loc: *loc }
            }
        };

        self.cast_to_maybe_type(to_type, resolved_expr)
    }

    fn resolve_stmt(&mut self, stmt: &ast::Stmt<'a>) -> Stmt<'a> {
        match stmt {
            ast::Stmt::Decl { name, typ, value, loc } => {
                match typ {
                    Some(t) => {
                        let typ = self.resolve_type(t);
                        let resolved_value = self.resolve_expr(value, Some(typ.clone()));
                        self.is_never(&resolved_value);
                        let decl = self.add_name(name.clone(), NameInfo::Local { typ, loc: *loc, level: self.level });
                        Stmt::Decl { decl, value: resolved_value, loc: *loc }
                    },
                    None => {
                        let resolved_value = self.resolve_expr(value, None);
                        self.is_never(&resolved_value);
                        let decl = self.add_name(name.clone(), NameInfo::Local { typ: self.type_of(&resolved_value), loc: *loc, level: self.level });
                        Stmt::Decl { decl, value: resolved_value, loc: *loc }
                    }
                }
            },
            ast::Stmt::Expr { expr, loc } => {
                Stmt::Expr { expr: self.resolve_expr(expr, None), loc: *loc }
            },
            ast::Stmt::Return { value, loc} => {
                let expected_return = self.expected_return.clone();
                let resolved_value = self.resolve_expr(value, expected_return);

                self.return_types.push(self.checker.hir.type_of_expr(&resolved_value));

                self.is_never(&resolved_value);
                Stmt::Return { value: resolved_value, loc: *loc }
                // let value = self.resolve_expr(value, None);
                // self.check(&self.checker.hir.type_of_expr(&value), Some(expected_return), loc);
                // Stmt::Return { value, loc: *loc }
            }
        }
    }
}
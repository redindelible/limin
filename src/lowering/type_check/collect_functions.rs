use std::collections::HashMap;
use std::path::PathBuf;
use slotmap::SecondaryMap;
use crate::parsing::ast;
use crate::lowering::type_check::{TypeCheck, NamespaceKey, TypeCheckError, resolve_type};
use crate::lowering::hir::*;
use crate::lowering::type_check::collect_fields::CollectedFields;

pub struct CollectedFunctions<'a> {
    pub(super) checker: TypeCheck<'a>,

    pub root: NamespaceKey,
    pub files: HashMap<PathBuf, ast::File<'a>>,
    pub file_namespaces: HashMap<PathBuf, NamespaceKey>,
    pub function_namespaces: SecondaryMap<FunctionKey, NamespaceKey>,
    pub struct_namespaces: SecondaryMap<StructKey, NamespaceKey>,
}

pub fn collect_functions(collected: CollectedFields) -> CollectedFunctions {
    let CollectedFields { mut checker, files, file_namespaces, root, struct_namespaces } = collected;

    let mut func_namespaces = SecondaryMap::new();

    for (file_path, file) in &files {
        let file_ns = file_namespaces[file_path];
        for top_level in &file.top_levels {
            match top_level {
                ast::TopLevel::Function(func) => {
                    let func_ns = checker.add_namespace(Some(file_ns));

                    let mut type_params = vec![];
                    for type_param in &func.type_parameters {
                        let bound = if let Some(bound) = &type_param.bound {
                            let typ = resolve_type(&checker, file_ns, bound);
                            Some(typ)
                        } else {
                            None
                        };
                        let id = checker.add_type_param();
                        let typ = Type::TypeParameter { name: type_param.name.clone(), bound: bound.clone().map(|t| Box::new(t)), id };
                        type_params.push(TypeParameter { name: type_param.name.clone(), bound, id });
                        checker.add_type(func_ns, type_param.name.clone(), typ);
                    }

                    let mut params = Vec::new();
                    for param in &func.parameters {
                        let typ = resolve_type(&checker, func_ns, &param.typ);
                        let decl = checker.add_name(func_ns, param.name.clone(), NameInfo::Local { typ: typ.clone(), loc: param.loc, level: 0 });
                        params.push(Parameter { name: param.name.clone(), typ, loc: param.loc, decl });
                    }
                    let ret = func.return_type.as_ref().map_or(Type::Unit, |t| resolve_type(&checker, func_ns, t));

                    let key = checker.hir.function_prototypes.insert_with_key(|key| {
                        let decl = checker.hir.names.insert(NameInfo::Function { func: key });
                        checker.namespaces[file_ns].names.insert(func.name.clone(), decl);
                        let sig = Type::GenericFunction { func: key, type_params: type_params.clone(), params: params.iter().map(|p| p.typ.clone()).collect(), ret: Box::new(ret.clone()) };
                        FunctionPrototype { name: func.name.clone(), type_params, params, ret, sig, decl, loc: func.loc }
                    });

                    if func.name == "main" {
                        let proto = &checker.hir.function_prototypes[key];
                        if !proto.params.is_empty() {
                            checker.push_error(TypeCheckError::MainMustHaveNoArguments(func.loc))
                        }
                        if !matches!(proto.ret, Type::Integer { bits: 32 }) {
                            checker.push_error(TypeCheckError::MainMustReturnI32(func.loc))
                        }

                        if let Some(prev_func) = checker.hir.main_function {
                            let prev_loc = checker.hir.function_prototypes[prev_func].loc;
                            checker.push_error(TypeCheckError::MultipleMainFunctions(func.loc, prev_loc))
                        } else {
                            checker.hir.main_function = Some(key);
                        }
                    }

                    // checker.add_name(file_ns, func.name.clone(), NameInfo::Function { func: key });
                    func_namespaces.insert(key, func_ns);
                }
                ast::TopLevel::Struct(struct_) => {
                    let key = checker.get_struct_key(file_ns, &struct_.name);
                    let struct_ns = struct_namespaces[key];
                    let self_type = Type::Struct {
                        struct_: key,
                        variant: checker.hir.structs[key].type_params.iter().map(|tp| tp.as_type()).collect()
                    };

                    for item in &struct_.items {
                        if let ast::StructItem::Impl(impl_) = item {
                            match impl_ {
                                ast::Impl::Unbounded { methods, loc } => {
                                    let mut im = Impl {
                                        impl_trait: None,
                                        bounds: vec![],
                                        method_prototypes: HashMap::new(),
                                        method_bodies: HashMap::new(),
                                        loc: *loc
                                    };
                                    for method in methods {
                                        let func_ns = checker.add_namespace(Some(struct_ns));

                                        let mut type_params = vec![];
                                        for type_param in &method.type_parameters {
                                            let bound = if let Some(bound) = &type_param.bound {
                                                let typ = resolve_type(&checker, struct_ns, bound);
                                                Some(typ)
                                            } else {
                                                None
                                            };
                                            let id = checker.add_type_param();
                                            let typ = Type::TypeParameter { name: type_param.name.clone(), bound: bound.clone().map(|t| Box::new(t)), id };
                                            type_params.push(TypeParameter { name: type_param.name.clone(), bound, id });
                                            checker.add_type(func_ns, type_param.name.clone(), typ);
                                        }

                                        let maybe_self = if let Some((name, loc)) = &method.maybe_self {
                                            let self_decl = checker.add_name(func_ns, name.clone(), NameInfo::Local {
                                                typ: self_type.clone(),
                                                level: 0,
                                                loc: *loc
                                            });
                                            Some(self_decl)
                                        } else {
                                            None
                                        };

                                        let mut params = Vec::new();
                                        for param in &method.parameters {
                                            let typ = resolve_type(&checker, func_ns, &param.typ);
                                            let decl = checker.add_name(func_ns, param.name.clone(), NameInfo::Local { typ: typ.clone(), loc: param.loc, level: 0 });
                                            params.push(Parameter { name: param.name.clone(), typ, loc: param.loc, decl });
                                        }
                                        let ret = method.return_type.as_ref().map_or(Type::Unit, |t| resolve_type(&checker, func_ns, t));

                                        let proto = MethodPrototype {
                                            name: method.name.clone(),
                                            type_params,
                                            maybe_self,
                                            params,
                                            ret,
                                            loc: *loc
                                        };
                                        im.method_prototypes.insert(method.name.clone(), proto);
                                    }
                                    checker.hir.structs[key].impls.push(im);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    if checker.hir.main_function.is_none() {
        checker.push_error(TypeCheckError::NoMainFunction);
    }

    CollectedFunctions { checker, files, file_namespaces, function_namespaces: func_namespaces, root, struct_namespaces }
}
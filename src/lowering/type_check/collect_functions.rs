use indexmap::IndexMap;
use crate::parsing::ast;
use crate::lowering::type_check as tc;
use crate::source::Location;

pub(super) struct StructInfo<'a> {
    pub fields: IndexMap<String, FieldInfo<'a>>,
    pub impls: Vec<ImplInfo<'a>>,

    pub self_type: tc::Type,
}

pub(super) struct FieldInfo<'a> {
    pub ty: tc::Type,
    pub loc: Location<'a>
}

pub(super) struct ImplInfo<'a> {
    pub methods: IndexMap<String, MethodInfo<'a>>,
    pub loc: Location<'a>
}

pub(super) struct MethodInfo<'a> {
    type_parameters: IndexMap<String, tc::collect_types::TypeParameterInfo<'a>>,
    maybe_self: Option<tc::NameKey>,
    params: IndexMap<String, ParameterInfo<'a>>,
    ret: tc::Type,

    ast_method: &'a ast::Method<'a>
}

pub(super) struct ParameterInfo<'a> {
    ty: tc::Type,
    loc: Location<'a>
}

pub(super) struct FunctionInfo<'a> {
    pub type_parameters: IndexMap<String, tc::collect_types::TypeParameterInfo<'a>>,
    pub params: IndexMap<String, ParameterInfo<'a>>,
    pub ret: tc::Type,

    pub ns: tc::NamespaceKey,
    pub name_key: tc::NameKey,
    pub ast_func: &'a ast::Function<'a>
}

pub(super) struct CollectedPrototypes<'a> {
    pub types: tc::CollectedTypes<'a>,
    pub functions: IndexMap<tc::FunctionKey, FunctionInfo<'a>>,
    pub structs: IndexMap<tc::StructKey, StructInfo<'a>>
}

pub(super) fn collect_functions<'a>(checker: &mut tc::TypeCheck<'a>, types: tc::collect_types::CollectedTypes<'a>) -> CollectedPrototypes<'a> {
    let tc::CollectedTypes { file_info, structs, .. } = &types;

    let mut functions: IndexMap<tc::FunctionKey, FunctionInfo> = IndexMap::new();
    let mut function_key = tc::FunctionKey::default();
    let mut main_key: Option<tc::FunctionKey> = None;

    let mut struct_info: IndexMap<tc::StructKey, StructInfo> = IndexMap::new();

    for &tc::collect_types::FileInfo { file_ns, ast_file, .. } in file_info.iter() {
        for top_level in &ast_file.top_levels {
            match top_level {
                ast::TopLevel::Function(func) => {
                    let func_ns = checker.add_namespace(Some(file_ns));

                    let mut type_parameters: IndexMap<String, tc::collect_types::TypeParameterInfo> = IndexMap::new();
                    for type_param in &func.type_parameters {
                        let bound = if let Some(bound) = &type_param.bound {
                            let typ = checker.resolve_type(bound, func_ns, &types);
                            Some(typ)
                        } else {
                            None
                        };
                        let key = checker.add_type_param();
                        type_parameters.insert(type_param.name.clone(), tc::collect_types::TypeParameterInfo { key, loc: type_param.loc });
                        checker.add_type(func_ns, type_param.name.clone(), tc::Type::TypeParameter(key));
                    }

                    let mut params: IndexMap<String, ParameterInfo> = IndexMap::new();
                    for param in &func.parameters {
                        let typ = checker.resolve_type(&param.typ, func_ns, &types).expect_type(checker);
                        let (decl, prev) = checker.add_local(&param.name, typ.clone(), param.loc, func_ns);
                        params.insert(param.name.clone(), ParameterInfo { ty: typ, loc: param.loc });
                    }
                    let ret = match &func.return_type {
                        Some(ty) => checker.resolve_type(ty, func_ns, &types).expect_type(checker),
                        None => tc::Type::Unit
                    };

                    let (name_key, prev) = checker.add_function(
                        &func.name, function_key,
                        params.iter().map(|(_, p)| p.ty.clone()).collect(), ret.clone(),
                        func.loc, file_ns
                    );
                    if let Some(prev_key) = prev {
                        let prev_info = checker.get_name(prev_key);
                        checker.push_error(tc::TypeCheckError::NameDuplicated(prev_info.name().clone(), func.loc, prev_info.loc()));
                    }
                    functions.insert(function_key, FunctionInfo {
                        type_parameters,
                        params,
                        ret,
                        ns: func_ns,
                        name_key,
                        ast_func: func
                    });

                    if func.name == "main" {
                        let info = &functions[&function_key];
                        if !info.params.is_empty() {
                            checker.push_error(tc::TypeCheckError::MainMustHaveNoArguments(func.loc))
                        }
                        if !matches!(info.ret, tc::Type::SignedInteger(32)) {
                            checker.push_error(tc::TypeCheckError::MainMustReturnI32(func.loc))
                        }

                        if let Some(prev_func) = main_key {
                            checker.push_error(tc::TypeCheckError::MultipleMainFunctions(func.loc, functions[&prev_func].ast_func.loc))
                        } else {
                            main_key = Some(function_key);
                        }
                    }

                    function_key = function_key.next();
                }
                _ => { }
            }
        }
    }

    for (&struct_key, struct_info) in structs {
        let &tc::collect_types::StructInfo { ref name, struct_ns, super_struct, ref type_parameters, ast_struct } = struct_info;

        let self_type = tc::Type::Struct(struct_key, type_parameters.values().map(|t| t.as_type()).collect());

        let mut fields: IndexMap<String, FieldInfo> = IndexMap::new();
        let mut impls: Vec<ImplInfo> = Vec::new();

        for item in &ast_struct.items {
            match item {
                ast::StructItem::Field { name: field_name, typ, loc } => {
                    let resolved = checker.resolve_type(typ, struct_ns, &types).expect_type(checker);
                    if let Some(prev) = fields.insert(field_name.clone(), FieldInfo { ty: resolved, loc: *loc}) {
                        checker.push_error(tc::TypeCheckError::FieldDuplicated(field_name.clone(), name.clone(), *loc, prev.loc));
                    }
                }
                ast::StructItem::Impl(ast::Impl::Unbounded { methods: ast_methods, loc }) => {
                    let mut methods: IndexMap<String, MethodInfo> = IndexMap::new();

                    for func in ast_methods {
                        let func_ns = checker.add_namespace(Some(struct_ns));

                        let mut type_parameters: IndexMap<String, tc::collect_types::TypeParameterInfo> = IndexMap::new();
                        for type_param in &func.type_parameters {
                            let bound = if let Some(bound) = &type_param.bound {
                                let typ = checker.resolve_type(bound, func_ns, &types);
                                Some(typ)
                            } else {
                                None
                            };
                            let key = checker.add_type_param();
                            type_parameters.insert(type_param.name.clone(), tc::collect_types::TypeParameterInfo { key, loc: type_param.loc });
                            checker.add_type(func_ns, type_param.name.clone(), tc::Type::TypeParameter(key));
                        }

                        let maybe_self = if let Some((name, loc)) = &func.maybe_self {
                            let (self_decl, _) = checker.add_local(name, self_type.clone(), *loc, func_ns);
                            Some(self_decl)
                        } else {
                            None
                        };

                        let mut params: IndexMap<String, ParameterInfo> = IndexMap::new();
                        for param in &func.parameters {
                            let typ = checker.resolve_type(&param.typ, func_ns, &types).expect_type(checker);
                            let (decl, prev) = checker.add_local(&param.name, typ.clone(), param.loc, func_ns);
                            params.insert(param.name.clone(), ParameterInfo { ty: typ, loc: param.loc });
                        }
                        let ret = match &func.return_type {
                            Some(ty) => checker.resolve_type(ty, func_ns, &types).expect_type(checker),
                            None => tc::Type::Unit
                        };

                        let prev_method = methods.insert(func.name.clone(), MethodInfo {
                            type_parameters,
                            maybe_self,
                            params,
                            ret,
                            ast_method: func
                        });
                        if let Some(prev_method) = prev_method {
                            checker.push_error(tc::TypeCheckError::MethodDuplicated(func.name.clone(), struct_info.name.clone(), func.loc, prev_method.ast_method.loc));
                        }
                    }
                    impls.push(ImplInfo {
                        methods,
                        loc: *loc
                    });
                }
            }
        }
    }

    if main_key.is_none() {
        checker.push_error(tc::TypeCheckError::NoMainFunction);
    }

    CollectedPrototypes { types, functions, structs: struct_info }
}

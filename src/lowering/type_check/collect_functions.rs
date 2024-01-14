use indexmap::IndexMap;
use crate::parsing::ast;
use crate::lowering::type_check as tc;
use crate::source::Location;
use crate::util::KeyMap;

pub(super) struct StructInfo<'a, 'b> {
    pub type_parameters: IndexMap<String, tc::TypeParameterKey>,
    pub fields: IndexMap<String, FieldInfo<'a>>,

    pub self_type: tc::StructType,
    pub ast_struct: &'b ast::Struct<'a>
}

#[derive(Clone)]
pub(super) struct FieldInfo<'a> {
    pub ty: tc::Type,
    pub loc: Location<'a>
}

pub(super) struct ImplInfo<'a> {
    pub methods: IndexMap<String, tc::MethodKey>,
    pub loc: Location<'a>
}

pub(super) struct MethodInfo<'a, 'b> {
    pub type_parameters: IndexMap<String, tc::TypeParameterKey>,
    pub maybe_self: Option<tc::NameKey>,
    pub params: IndexMap<String, ParameterInfo<'a>>,
    pub ret: tc::Type,

    pub ns: tc::NamespaceKey,
    pub ast_method: &'b ast::Method<'a>
}

pub(super) struct ParameterInfo<'a> {
    pub ty: tc::Type,
    pub decl: tc::NameKey,
    pub loc: Location<'a>
}

pub(super) struct FunctionInfo<'a, 'b> {
    pub type_parameters: IndexMap<String, tc::TypeParameterKey>,
    pub params: IndexMap<String, ParameterInfo<'a>>,
    pub ret: tc::Type,

    pub ns: tc::NamespaceKey,
    pub name_key: tc::NameKey,
    pub ast_func: &'b ast::Function<'a>
}

impl FunctionInfo<'_, '_> {
    pub fn typ(&self) -> Option<tc::FunctionType> {
        if self.type_parameters.is_empty() {
            let param_types = self.params.values().map(|p| p.ty.clone()).collect();
            Some(tc::FunctionType(param_types, Box::new(self.ret.clone())))
        } else {
            None
        }
    }
}

pub(super) struct CollectedPrototypes<'a, 'b> {
    pub types: tc::CollectedTypes<'a, 'b>,
    pub functions: KeyMap<tc::FunctionKey, FunctionInfo<'a, 'b>>,
    pub structs: KeyMap<tc::StructKey, StructInfo<'a, 'b>>,
    pub methods: KeyMap<tc::MethodKey, MethodInfo<'a, 'b>>,
    pub main_function: Option<tc::FunctionKey>,
}

impl<'a, 'b> CollectedPrototypes<'a, 'b> {
    pub fn get_method(&self, struct_: tc::StructKey, name: impl AsRef<str>) -> Vec<tc::MethodKey> {
        let mut methods = Vec::new();

        for impl_ in &self.structs[struct_].impls {
            if let Some(method_key) = impl_.methods.get(name.as_ref()) {
                methods.push(*method_key)
            }
        }

        methods
    }
}

pub(super) fn collect_functions<'a, 'b>(checker: &mut tc::TypeCheck<'a>, types: tc::CollectedTypes<'a, 'b>) -> CollectedPrototypes<'a, 'b> {
    let tc::CollectedTypes { file_info, structs, .. } = &types;

    let mut functions: KeyMap<tc::FunctionKey, FunctionInfo> = KeyMap::new();
    let mut main_key: Option<tc::FunctionKey> = None;

    let mut collected_structs: KeyMap<tc::StructKey, StructInfo> = KeyMap::new();
    let mut methods: KeyMap<tc::MethodKey, MethodInfo> = KeyMap::new();

    for &tc::collect_structs::FileInfo { file_ns, ast_file, .. } in file_info.iter() {
        for top_level in &ast_file.top_levels {
            match top_level {
                ast::TopLevel::Function(func) => {
                    let func_ns = checker.add_namespace(Some(file_ns));
                    let function_key = functions.reserve();

                    let mut type_parameters: IndexMap<String, tc::TypeParameterKey> = IndexMap::new();
                    for type_param in &func.type_parameters {
                        let key = checker.add_type_param(type_param.name.clone(), type_param.loc);
                        type_parameters.insert(type_param.name.clone(), key);
                        checker.add_type(func_ns, type_param.name.clone(), tc::Type::TypeParameter(key));
                    }

                    let mut params: IndexMap<String, ParameterInfo> = IndexMap::new();
                    for param in &func.parameters {
                        let typ = checker.resolve_type(&param.typ, func_ns, &types).expect_type(checker);
                        let (decl, prev) = checker.add_local(&param.name, typ.clone(), 0, param.loc, func_ns);
                        params.insert(param.name.clone(), ParameterInfo { ty: typ, decl, loc: param.loc });
                    }
                    let ret = match &func.return_type {
                        Some(ty) => checker.resolve_type(ty, func_ns, &types).expect_type(checker),
                        None => tc::Type::Unit
                    };

                    let (name_key, prev) = checker.add_function(&func.name, function_key, func.loc, file_ns);
                    if let Some(prev_key) = prev {
                        let prev_info = &checker.names[prev_key];
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
                        let info = &functions[function_key];
                        if !info.params.is_empty() {
                            checker.push_error(tc::TypeCheckError::MainMustHaveNoArguments(func.loc))
                        }
                        if !matches!(info.ret, tc::Type::SignedInteger(32)) {
                            checker.push_error(tc::TypeCheckError::MainMustReturnI32(func.loc))
                        }

                        if let Some(prev_func) = main_key {
                            checker.push_error(tc::TypeCheckError::MultipleMainFunctions(func.loc, functions[prev_func].ast_func.loc))
                        } else {
                            main_key = Some(function_key);
                        }
                    }
                }
                _ => { }
            }
        }
    }

    for (struct_key, struct_info) in structs {
        let &tc::collect_structs::StructInfo { ref name, struct_ns, super_struct, ref type_parameters, ast_struct } = struct_info;

        let self_type = tc::StructType(struct_key, type_parameters.values().map(|&t| t.into()).collect());

        let mut fields: IndexMap<String, FieldInfo> = IndexMap::new();

        for item in &ast_struct.items {
            match item {
                ast::StructItem::Field { name: field_name, typ, loc } => {
                    let resolved = checker.resolve_type(typ, struct_ns, &types).expect_type(checker);
                    if let Some(prev) = fields.insert(field_name.clone(), FieldInfo { ty: resolved, loc: *loc}) {
                        checker.push_error(tc::TypeCheckError::FieldDuplicated(field_name.clone(), name.clone(), *loc, prev.loc));
                    }
                }
            }
        }

        collected_structs.insert(struct_key, StructInfo {
            type_parameters: type_parameters.clone(),
            fields,
            self_type,
            ast_struct
        });
    }

    if main_key.is_none() {
        checker.push_error(tc::TypeCheckError::NoMainFunction);
    }

    CollectedPrototypes { types, functions, structs: collected_structs, methods, main_function: main_key }
}

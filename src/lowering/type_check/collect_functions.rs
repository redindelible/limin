use std::collections::HashMap;
use indexmap::IndexMap;
use crate::parsing::ast;
use crate::lowering::{hir, type_check as tc};
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

pub(super) struct ImplInfo<'a, 'b> {
    pub for_type: tc::Type,

    pub methods: IndexMap<String, tc::MethodKey>,
    pub loc: Location<'a>,
    pub ast_impl: &'b ast::Impl<'a>,
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
    pub impls: KeyMap<tc::ImplKey, ImplInfo<'a, 'b>>,
    pub methods: KeyMap<tc::MethodKey, MethodInfo<'a, 'b>>,
    pub main_function: Option<tc::FunctionKey>,
}


impl<'a, 'b> CollectedPrototypes<'a, 'b> {
    fn unify(&self, check_ty: &tc::Type, impl_ty: &tc::Type, inference_map: &mut HashMap<tc::InferenceVariableKey, tc::Type>) -> bool {
        use tc::{Type, StructType, FunctionType};

        match (check_ty, impl_ty) {
            (Type::Unit, Type::Unit) => true,
            (Type::Boolean, Type::Boolean) => true,
            (Type::SignedInteger(a_bits), Type::UnsignedInteger(b_bits)) => a_bits == b_bits,
            (Type::UnsignedInteger(a_bits), Type::UnsignedInteger(b_bits)) => a_bits == b_bits,
            (Type::Struct(StructType(a_struct, a_variant)), Type::Struct(StructType(b_struct, b_variant))) => {
                if a_struct != b_struct {
                    return false;
                }
                if a_variant.len() != b_variant.len() {
                    return false;
                }

                a_variant.iter().zip(b_variant).all(|(a, b)| self.unify(a, b, inference_map))
            }

            (Type::Function(FunctionType(a_params, a_ret)), Type::Function(FunctionType(b_params, b_ret))) => {
                if a_params.len() != b_params.len() {
                    return false;
                }

                a_params.iter().zip(b_params).all(|(a, b)| self.unify(a, b, inference_map)) && self.unify(a_ret, b_ret, inference_map)
            }
            (Type::TypeParameter(a_key), Type::TypeParameter(b_key)) => {
                a_key == b_key
            }
            (Type::InferenceVariable(a_key), impl_ty) => {
                unreachable!("Shouldn't be allowed")
            }
            (check_ty, Type::InferenceVariable(key)) => {
                if let Some(inferred_ty) = inference_map.get(key) {
                    self.unify(check_ty, &inferred_ty.clone(), inference_map)
                } else {
                    inference_map.insert(*key, check_ty.clone());
                    true
                }
            }
            _ => false
        }
    }

    pub fn get_impls(&self, for_type: &tc::Type) -> Vec<tc::ImplKey> {
        let mut impls = Vec::new();

        for (impl_key, impl_info) in &self.impls {
            let mut type_parameters = HashMap::new();
            if self.unify(for_type, &impl_info.for_type, &mut type_parameters) {
                impls.push(impl_key);
            }
        }

        impls
    }

    pub fn get_method(&self, for_type: &tc::Type, method: &str) -> Vec<tc::MethodKey> {
        let mut keys = Vec::new();

        for (impl_key, impl_info) in &self.impls {
            let mut type_parameters = HashMap::new();
            if self.unify(for_type, &impl_info.for_type, &mut type_parameters) {
                if let Some(key) = impl_info.methods.get(method) {
                    keys.push(*key)
                }
            }
        }

        keys
    }
}


pub(super) fn collect_functions<'a, 'b>(checker: &mut tc::TypeCheck<'a>, types: tc::CollectedTypes<'a, 'b>) -> CollectedPrototypes<'a, 'b> {
    let tc::CollectedTypes { file_info, structs, .. } = &types;

    let mut functions: KeyMap<tc::FunctionKey, FunctionInfo> = KeyMap::new();
    let mut main_key: Option<tc::FunctionKey> = None;

    let mut collected_structs: KeyMap<tc::StructKey, StructInfo> = KeyMap::new();

    let mut impls: KeyMap<tc::ImplKey, ImplInfo> = KeyMap::new();
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

                ast::TopLevel::Impl(ast_impl @ ast::Impl::Inherent { ref for_type, methods: ref ast_methods, ref loc }) => {
                    let impl_ns = checker.add_namespace(Some(file_ns));
                    let for_type = checker.resolve_type(for_type, file_ns, &types).expect_type(checker);

                    let impl_methods = ast_methods.iter().map(|method| {
                        let method_ns = checker.add_namespace(Some(impl_ns));

                        let type_parameters = method.type_parameters.iter().map(|tp| {
                            let key = checker.add_type_param(tp.name.clone(), tp.loc);
                            checker.add_type(method_ns, tp.name.clone(), tc::Type::TypeParameter(key));
                            (tp.name.clone(), key)
                        }).collect();

                        let maybe_self = if let Some((name, loc)) = &method.maybe_self {
                            let (self_key, _) = checker.add_local(name, for_type.clone(), 0, *loc, method_ns);
                            Some(self_key)
                        } else {
                            None
                        };

                        let params = method.parameters.iter().map(|param| {
                            let ty = checker.resolve_type(&param.typ, method_ns, &types).expect_type(checker);
                            let (decl, prev) = checker.add_local(&param.name, ty.clone(), 0, param.loc, method_ns);
                            (param.name.clone(), ParameterInfo {
                                ty,
                                decl,
                                loc: param.loc
                            })
                        }).collect();

                        let ret = match &method.return_type {
                            Some(ty) => checker.resolve_type(ty, method_ns, &types).expect_type(checker),
                            None => tc::Type::Unit
                        };

                        let method_key = methods.add(MethodInfo {
                            type_parameters,
                            maybe_self,
                            params,
                            ret,
                            ns: method_ns,
                            ast_method: method
                        });

                        (method.name.clone(), method_key)
                    }).collect();

                    impls.add(ImplInfo {
                        for_type,
                        methods: impl_methods,
                        loc: *loc,
                        ast_impl
                    });
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

    CollectedPrototypes { types, functions, structs: collected_structs, impls, methods, main_function: main_key }
}

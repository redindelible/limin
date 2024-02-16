use std::collections::HashMap;
use indexmap::IndexMap;
use crate::parsing::ast;
use crate::lowering::{type_check as tc};
use crate::source::Location;
use crate::util::{KeyMap};

pub(super) struct TraitInfo<'a, 'b> {
    pub type_parameters: IndexMap<String, tc::TypeParameterKey>,
    pub methods: IndexMap<String, MethodPrototype<'a>>,

    pub self_type: tc::TraitType,
    pub ast_trait: &'b ast::Trait<'a>
}

#[derive(Clone)]
pub(super) struct MethodPrototype<'a> {
    pub name: String,
    pub has_self: bool,
    pub params: Vec<tc::Type>,
    pub ret: tc::Type,
    pub loc: Location<'a>
}

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

#[derive(Debug)]
pub(super) struct ImplInfo<'a, 'b> {
    pub type_parameters: IndexMap<String, tc::TypeParameterKey>,
    pub trait_: Option<tc::TraitKey>,
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
    pub traits: KeyMap<tc::TraitKey, TraitInfo<'a, 'b>>,
    pub impls: KeyMap<tc::ImplKey, ImplInfo<'a, 'b>>,
    pub methods: KeyMap<tc::MethodKey, MethodInfo<'a, 'b>>,
    pub main_function: Option<tc::FunctionKey>,
}


impl<'a, 'b> CollectedPrototypes<'a, 'b> {
    fn unify(&self, check_ty: &tc::Type, impl_ty: &tc::Type, inference_map: &mut HashMap<tc::TypeParameterKey, Option<tc::Type>>) -> bool {
        use tc::{Type, StructType, TraitType, FunctionType};

        match (check_ty, impl_ty) {
            (Type::Unit, Type::Unit) => true,
            (Type::Boolean, Type::Boolean) => true,
            (Type::SignedInteger(a_bits), Type::SignedInteger(b_bits)) => a_bits == b_bits,
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
            (Type::Trait(TraitType(a_trait, a_variant)), Type::Trait(TraitType(b_trait, b_variant))) => {
                if a_trait != b_trait {
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
            (a_type, Type::TypeParameter(b_key)) => {
                if let Some(maybe_defined) = inference_map.get_mut(b_key) {
                    if let Some(defined) = maybe_defined {
                        return self.unify(a_type, &defined.clone(), inference_map);
                    } else {
                        *maybe_defined = Some(a_type.clone());
                        return true;
                    }
                } else {
                    if let Type::TypeParameter(a_key) = a_type {
                        return a_key == b_key
                    } else {
                        false
                    }
                }
            }
            _ => false
        }
    }

    // pub fn get_impls(&self, for_type: &tc::Type) -> Vec<tc::ImplKey> {
    //     let mut impls = Vec::new();
    //
    //     for (impl_key, impl_info) in &self.impls {
    //         let mut type_parameters = HashMap::new();
    //         if self.unify(for_type, &impl_info.for_type, &mut type_parameters) {
    //             impls.push(impl_key);
    //         }
    //     }
    //
    //     impls
    // }

    pub fn get_method(&self, for_type: &tc::Type, method: &str) -> Vec<(tc::MethodKey, tc::TypeMap)> {
        let mut keys = Vec::new();

        for (impl_key, impl_info) in &self.impls {
            let mut inference_map = HashMap::new();
            for tp in impl_info.type_parameters.values() {
                inference_map.insert(*tp, None);
            }

            if self.unify(for_type, &impl_info.for_type, &mut inference_map) {

                let mut type_map = HashMap::new();
                for (tp, typ) in inference_map {
                    type_map.insert(tp, typ.unwrap());
                }

                if let Some(key) = impl_info.methods.get(method) {
                    keys.push((*key, type_map))
                }
            }
        }

        keys
    }
}


pub(super) fn collect_functions<'a, 'b>(checker: &mut tc::TypeCheck<'a>, types: tc::CollectedTypes<'a, 'b>) -> CollectedPrototypes<'a, 'b> {
    let tc::CollectedTypes { file_info, structs, traits, .. } = &types;

    let mut functions: KeyMap<tc::FunctionKey, FunctionInfo> = KeyMap::new();
    let mut main_key: Option<tc::FunctionKey> = None;

    let mut collected_structs: KeyMap<tc::StructKey, StructInfo> = KeyMap::new();
    for (struct_key, struct_info) in structs {
        let &tc::collect_structs::StructInfo {
            ref name, struct_ns,
            ref type_parameters, ast_struct, ..
        } = struct_info;

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

    let mut collected_traits = KeyMap::new();
    for (trait_key, trait_info) in traits {
        let self_type = tc::TraitType(trait_key, trait_info.type_parameters.values().map(|&t| t.into()).collect());

        let methods = trait_info.ast_trait.method_prototypes.iter().map(|method| {
            let params = method.parameters.iter().map(
                |param| checker.resolve_type(&param.typ, trait_info.trait_ns, &types).expect_type(checker)
            ).collect();

            let ret = method.return_type.as_ref().map_or(tc::Type::Unit,
                |typ| checker.resolve_type(typ, trait_info.trait_ns, &types).expect_type(checker)
            );

            (method.name.clone(), MethodPrototype {
                name: method.name.clone(),
                has_self: method.has_self,
                params,
                ret,
                loc: trait_info.ast_trait.loc
            })
        }).collect();

        collected_traits.insert(trait_key, TraitInfo {
            type_parameters: trait_info.type_parameters.clone(),
            methods,
            self_type,
            ast_trait: trait_info.ast_trait
        });
    }

    let mut impls: KeyMap<tc::ImplKey, ImplInfo> = KeyMap::new();
    let mut methods: KeyMap<tc::MethodKey, MethodInfo> = KeyMap::new();

    for &tc::collect_struct_prototypes::FileInfo { file_ns, ast_file } in file_info.iter() {
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

                ast::TopLevel::Impl(ast_impl @ ast::Impl { ref type_parameters, ref trait_, ref for_type, methods: ref ast_methods, ref loc }) => {
                    let impl_ns = checker.add_namespace(Some(file_ns));

                    let type_parameters = type_parameters.iter().map(|tp| {
                        let key = checker.add_type_param(tp.name.clone(), tp.loc);
                        checker.add_type(impl_ns, tp.name.clone(), tc::Type::TypeParameter(key));
                        (tp.name.clone(), key)
                    }).collect();

                    let trait_ = if let Some(trait_) = trait_ {
                        if let Some(trait_) = checker.resolve_trait(trait_, file_ns) {
                            Some(trait_)
                        } else {
                            todo!()
                        }
                    } else {
                        None
                    };

                    let mut required_methods: Option<Vec<String>> = trait_.map(|trait_key| {
                        collected_traits[trait_key].methods.keys().cloned().collect()
                    });

                    let for_type = checker.resolve_type(for_type, file_ns, &types).expect_type(checker);

                    let mut impl_methods = IndexMap::new();
                    for method in ast_methods.iter() {
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

                        let params: IndexMap<String, ParameterInfo> = method.parameters.iter().map(|param| {
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

                        if let Some(trait_key) = trait_ {
                            if let Some(method_proto) = collected_traits[trait_key].methods.get(&method.name) {
                                if method_proto.params.len() == params.len()
                                    && method_proto.has_self == maybe_self.is_some()
                                    && method_proto.params.iter().zip(params.values())
                                        .all(|(proto, info)| !checker.equate_types(proto, &info.ty, &types, method.loc).is_failure())
                                    && !checker.equate_types(&method_proto.ret, &ret, &types, method.loc).is_failure() {

                                    required_methods.as_mut().map(|required| {
                                        required.retain(|req| req != &method.name);
                                    });
                                } else {
                                    checker.push_error(tc::TypeCheckError::TraitMethodDiffers { got: method.loc, declared: method_proto.loc });
                                }
                            } else {
                                checker.push_error(tc::TypeCheckError::UndeclaredMethod {
                                    name: method.name.clone(),
                                    trait_: checker.display_type(&tc::Type::Trait(tc::TraitType(trait_key, vec![])), &types),
                                    loc: method.loc
                                });
                            }
                        }

                        let method_key = methods.add(MethodInfo {
                            type_parameters,
                            maybe_self,
                            params,
                            ret,
                            ns: method_ns,
                            ast_method: method
                        });

                        let maybe_prev = impl_methods.insert(method.name.clone(), method_key);
                        if let Some(prev) = maybe_prev {
                            checker.push_error(tc::TypeCheckError::MethodDuplicated(method.name.clone(), method.loc, methods[prev].ast_method.loc));
                        }
                    }

                    if let Some(required) = required_methods {
                        if !required.is_empty() {
                            checker.push_error(tc::TypeCheckError::MissingMethods {
                                methods: required,
                                trait_: checker.display_type(&tc::Type::Trait(tc::TraitType(trait_.unwrap(), vec![])), &types),
                                loc: ast_impl.loc
                            });
                        }
                    }


                    impls.add(ImplInfo {
                        type_parameters,
                        trait_,
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

    if main_key.is_none() {
        checker.push_error(tc::TypeCheckError::NoMainFunction);
    }

    CollectedPrototypes { types, functions, structs: collected_structs, traits: collected_traits, impls, methods, main_function: main_key }
}

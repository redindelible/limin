use std::collections::HashMap;
use std::path::PathBuf;
use indexmap::IndexMap;
use crate::parsing::ast;
use crate::lowering::type_check as tc;
use crate::util::KeyMap;

#[derive(Debug)]
pub(super) struct FileInfo<'a, 'b> {
    pub file_ns: tc::NamespaceKey,
    pub ast_file: &'b ast::File<'a>
}

pub(super) struct StructInfo<'a, 'b> {
    pub name: String,
    pub ast_struct: &'b ast::Struct<'a>,
    pub struct_ns: tc::NamespaceKey,
    pub type_parameters: IndexMap<String, tc::TypeParameterKey>
}

pub(super) struct TraitInfo<'a, 'b> {
    pub name: String,
    pub ast_trait: &'b ast::Trait<'a>,
    pub trait_ns: tc::NamespaceKey,
    pub type_parameters: IndexMap<String, tc::TypeParameterKey>
}

pub(super) struct CollectedStructPrototypes<'a, 'b> {
    pub file_info: HashMap<PathBuf, FileInfo<'a, 'b>>,
    pub structs: KeyMap<tc::StructKey, StructInfo<'a, 'b>>,
    pub traits: KeyMap<tc::TraitKey, TraitInfo<'a, 'b>>
}

pub(super) fn collect_struct_prototypes<'a, 'b>(checker: &mut tc::TypeCheck<'a>, ast: &'b ast::AST<'a>) -> CollectedStructPrototypes<'a, 'b> {
    let mut file_info = HashMap::new();
    let mut lib_root_namespaces = HashMap::new();
    for (lid_id, _) in &ast.libs {
        let lib_ns = checker.add_namespace(Some(checker.root()));
        lib_root_namespaces.insert(lid_id.clone(), lib_ns);
    }
    
    for (file_path, file) in &ast.files {
        let lib_ns = lib_root_namespaces[&file.in_lib];
        let file_ns = checker.add_namespace(Some(lib_ns));
        file_info.insert(file_path.clone(), FileInfo { file_ns, ast_file: file });
    }
    
    for (lid_id, lib_info) in &ast.libs {
        let lib_ns = lib_root_namespaces[lid_id];
        for (lib_dep_name, lib_dep_path) in &lib_info.libs {
            checker.namespaces[lib_ns].namespaces.insert(lib_dep_name.clone(), file_info[&ast.libs[lib_dep_path].root_path].file_ns);
        }
    }
    
    let mut structs: KeyMap<tc::StructKey, StructInfo> = KeyMap::new();
    let mut traits: KeyMap<tc::TraitKey, TraitInfo> = KeyMap::new();

    for (file_path, file) in &ast.files {
        let file_ns = file_info[file_path].file_ns;

        for top_level in &file.top_levels {
            match top_level {
                ast::TopLevel::Mod(ast::Mod { name, .. }) => {
                    let include_path = file_path.with_file_name(format!("{}.lmn", name));
                    let include_ns = file_info[&include_path].file_ns;
                    let prev = checker.namespaces[file_ns].namespaces.insert(name.to_owned(), include_ns);
                }
                ast::TopLevel::Struct(ast_struct) => {
                    let ast::Struct { name, loc, .. } = ast_struct;
                    let struct_key = structs.reserve();

                    if let Some(prev_key) = checker.add_struct(name, struct_key, file_ns) {
                        checker.push_error(tc::TypeCheckError::StructDuplicated(name.clone(), *loc, structs[prev_key].ast_struct.loc));
                    }

                    let struct_ns = checker.add_namespace(Some(file_ns));
                    let mut type_parameters: IndexMap<String, tc::TypeParameterKey> = IndexMap::new();

                    for type_param in &ast_struct.type_params {
                        let key = checker.add_type_param(type_param.name.clone(), type_param.loc);
                        checker.add_type(struct_ns, type_param.name.clone(), tc::Type::TypeParameter(key));
                        type_parameters.insert(type_param.name.clone(), key);
                    }

                    structs.insert(struct_key, StructInfo {
                        name: name.clone(),
                        struct_ns,
                        type_parameters,
                        ast_struct
                    });
                }
                ast::TopLevel::Trait(ast_trait) => {
                    let ast::Trait { name, .. } = ast_trait;
                    let trait_key = traits.reserve();

                    if let Some(prev_key) = checker.add_trait(name, trait_key, file_ns) {
                        todo!()
                    };
                    let trait_ns = checker.add_namespace(Some(file_ns));
                    let mut type_parameters: IndexMap<String, tc::TypeParameterKey> = IndexMap::new();
                    
                    for type_param in &ast_trait.type_parameters {
                        let key = checker.add_type_param(type_param.name.clone(), type_param.loc);
                        checker.add_type(trait_ns, type_param.name.clone(), tc::Type::TypeParameter(key));
                        type_parameters.insert(type_param.name.clone(), key);
                    }

                    traits.insert(trait_key, TraitInfo {
                        name: name.clone(),
                        trait_ns,
                        type_parameters,
                        ast_trait
                    });
                }
                _ => { }
            }
        }
    }

    CollectedStructPrototypes { structs, traits, file_info }
}
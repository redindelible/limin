use std::collections::HashMap;
use indexmap::IndexMap;
use crate::parsing::ast;
use crate::lowering::type_check as tc;
use crate::util::KeyMap;

pub(super) struct FileInfo<'a, 'b> {
    pub file_ns: tc::NamespaceKey,
    pub ast_file: &'b ast::File<'a>,
    pub structs: Vec<tc::StructKey>
}

pub(super) struct StructInfo<'a, 'b> {
    pub name: String,
    pub struct_ns: tc::NamespaceKey,
    pub ast_struct: &'b ast::Struct<'a>,
    pub super_struct: Option<tc::StructKey>,
    pub type_parameters: IndexMap<String, tc::TypeParameterKey>
}

pub(super) struct CollectedTypes<'a, 'b> {
    pub ast: &'b ast::AST<'a>,

    pub file_info: Vec<FileInfo<'a, 'b>>,
    pub structs: KeyMap<tc::StructKey, StructInfo<'a, 'b>>,
}

impl CollectedTypes<'_, '_> {
    pub fn create_subs(&self, tc::StructType(struct_, variant): tc::StructType) -> HashMap<tc::TypeParameterKey, tc::Type> {
        self.structs[struct_].type_parameters.values().copied().zip(variant).collect()
    }
}

pub(super) fn collect_structs<'a, 'b>(checker: &mut tc::TypeCheck<'a>, collected: tc::CollectedStructPrototypes<'a, 'b>) -> CollectedTypes<'a, 'b> {
    let tc::CollectedStructPrototypes { ast, structs, file_info} = collected;

    let mut new_file_info = Vec::new();
    for tc::collect_struct_prototypes::FileInfo { file_ns, structs, ast_file } in file_info {
        new_file_info.push(FileInfo { file_ns, structs, ast_file });
    }

    let mut new_structs = KeyMap::new();
    for (struct_key, tc::collect_struct_prototypes::StructInfo { name, containing: file_ns, ast_struct }) in structs {
        // let super_struct: Option<tc::StructKey> = if let Some((super_name, loc)) = &ast_struct.super_struct {
        //     checker.resolve_struct(super_name, file_ns, *loc).ok(checker)
        // } else {
        //     None
        // };
        let super_struct = None;

        let struct_ns = checker.add_namespace(Some(file_ns));
        let mut type_parameters: IndexMap<String, tc::TypeParameterKey> = IndexMap::new();

        for type_param in &ast_struct.type_params {
            let key = checker.add_type_param(type_param.name.clone(), type_param.loc);
            checker.add_type(struct_ns, type_param.name.clone(), tc::Type::TypeParameter(key));
            type_parameters.insert(type_param.name.clone(), key);
        }

        new_structs.insert(struct_key, StructInfo {
            name,
            struct_ns,
            ast_struct,
            super_struct,
            type_parameters
        });
    }

    CollectedTypes { ast, file_info: new_file_info, structs: new_structs }
}
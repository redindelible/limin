use indexmap::IndexMap;
use crate::parsing::ast;
use crate::source::Location;
use crate::lowering::type_check as tc;

pub(super) struct FileInfo<'a> {
    pub file_ns: tc::NamespaceKey,
    pub ast_file: &'a ast::File<'a>,
    pub structs: Vec<tc::StructKey>
}

pub(super) struct TypeParameterInfo<'a> {
    pub key: tc::TypeParameterKey,
    pub loc: Location<'a>
}

impl<'a> TypeParameterInfo<'a> {
    pub fn as_type(&self) -> tc::Type {
        tc::Type::TypeParameter(self.key)
    }
}

pub(super) struct StructInfo<'a> {
    pub name: String,
    pub struct_ns: tc::NamespaceKey,
    pub ast_struct: &'a ast::Struct<'a>,
    pub super_struct: Option<tc::StructKey>,
    pub type_parameters: IndexMap<String, TypeParameterInfo<'a>>
}

pub(super) struct CollectedTypes<'a> {
    pub ast: &'a ast::AST<'a>,

    pub file_info: Vec<FileInfo<'a>>,
    pub structs: IndexMap<tc::StructKey, StructInfo<'a>>,
}

pub(super) fn collect_types<'a>(checker: &mut tc::TypeCheck<'a>, collected: tc::CollectedStructs<'a>) -> CollectedTypes<'a> {
    let tc::CollectedStructs { ast, structs, file_info} = collected;

    let mut new_file_info = Vec::new();
    for tc::collect_structs::FileInfo { file_ns, structs, ast_file } in file_info {
        new_file_info.push(FileInfo { file_ns, structs, ast_file });
    }

    let mut new_structs = IndexMap::new();
    for (struct_key, tc::collect_structs::StructInfo { name, containing: file_ns, ast_struct }) in structs {
        let mut super_struct: Option<tc::StructKey>;
        if let Some((super_name, loc)) = &ast_struct.super_struct {
            super_struct = checker.resolve_struct(super_name, file_ns, *loc).ok(checker);
        } else {
            super_struct = None;
        }

        let struct_ns = checker.add_namespace(Some(file_ns));
        let mut type_parameters: IndexMap<String, TypeParameterInfo> = IndexMap::new();

        for type_param in &ast_struct.type_params {
            let key = checker.add_type_param();
            let typ = tc::Type::TypeParameter(key);
            checker.add_type(struct_ns, type_param.name.clone(), typ);
            type_parameters.insert(type_param.name.clone(), TypeParameterInfo { key, loc: type_param.loc });
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
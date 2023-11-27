use indexmap::IndexMap;
use crate::parsing::ast;
use crate::lowering::type_check as tc;

pub(super) struct FileInfo<'a> {
    file_ns: tc::NamespaceKey,
    ast_file: &'a ast::File<'a>,
    structs: Vec<tc::StructKey>
}

pub(super) struct StructInfo<'a> {
    name: String,
    containing: tc::NamespaceKey,
    ast_struct: &'a ast::Struct<'a>
}

pub(super) struct CollectedStructs<'a> {
    pub ast: &'a ast::AST<'a>,

    pub file_info: Vec<FileInfo<'a>>,
    pub structs: IndexMap<tc::StructKey, StructInfo<'a>>,
}

pub(super) fn collect_structs<'a>(checker: &mut tc::TypeCheck<'a>, ast: &'a ast::AST<'a>) -> CollectedStructs<'a> {
    let mut file_info = Vec::new();
    let mut structs: IndexMap<tc::StructKey, StructInfo> = IndexMap::new();
    let mut struct_key = tc::StructKey::default();

    for (_, file) in &ast.files {
        let file_ns = checker.add_namespace(Some(checker.root()));
        let mut file_structs = Vec::new();

        for ast_struct in file.iter_structs() {
            let ast::Struct { name, loc, .. } = ast_struct;
            if let Some(prev_key) = checker.add_struct(name, struct_key, file_ns) {
                checker.push_error(tc::TypeCheckError::StructDuplicated(name.clone(), *loc, structs[&prev_key].ast_struct.loc));
            }

            structs.insert(struct_key, StructInfo {
                name: name.clone(),
                containing: file_ns,
                ast_struct
            });
            file_structs.push(struct_key);
            struct_key = struct_key.next();
        }
        file_info.push(FileInfo { file_ns, structs: file_structs, ast_file: file });
    }

    CollectedStructs { ast, structs, file_info }
}
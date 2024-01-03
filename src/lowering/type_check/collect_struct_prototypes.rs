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
    pub containing: tc::NamespaceKey,
    pub ast_struct: &'b ast::Struct<'a>
}

pub(super) struct CollectedStructPrototypes<'a, 'b> {
    pub ast: &'b ast::AST<'a>,

    pub file_info: Vec<FileInfo<'a, 'b>>,
    pub structs: KeyMap<tc::StructKey, StructInfo<'a, 'b>>,
}

pub(super) fn collect_struct_prototypes<'a, 'b>(checker: &mut tc::TypeCheck<'a>, ast: &'b ast::AST<'a>) -> CollectedStructPrototypes<'a, 'b> {
    let mut file_info = Vec::new();
    let mut structs: KeyMap<tc::StructKey, StructInfo> = KeyMap::new();

    for (_, file) in &ast.files {
        let file_ns = checker.add_namespace(Some(checker.root()));
        let mut file_structs = Vec::new();

        for ast_struct in file.iter_structs() {
            let ast::Struct { name, loc, .. } = ast_struct;
            let struct_key = structs.reserve();
            
            if let Some(prev_key) = checker.add_struct(name, struct_key, file_ns) {
                checker.push_error(tc::TypeCheckError::StructDuplicated(name.clone(), *loc, structs[prev_key].ast_struct.loc));
            }

            structs.insert(struct_key, StructInfo {
                name: name.clone(),
                containing: file_ns,
                ast_struct
            });
            file_structs.push(struct_key);
        }
        file_info.push(FileInfo { file_ns, structs: file_structs, ast_file: file });
    }

    CollectedStructPrototypes { ast, structs, file_info }
}
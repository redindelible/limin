use std::collections::HashMap;
use std::path::PathBuf;
use indexmap::IndexMap;
use crate::parsing::ast;
use crate::lowering::type_check::{TypeCheck, NamespaceKey, Initial, TypeCheckError};
use crate::lowering::hir::*;

pub(super) struct CollectedStructs<'a> {
    pub(super) checker: TypeCheck<'a>,

    pub root: NamespaceKey,
    pub files: HashMap<PathBuf, ast::File<'a>>,
    pub file_namespaces: HashMap<PathBuf, NamespaceKey>,
}

pub(super) fn collect_structs(initial: Initial) -> CollectedStructs {
    let Initial { mut checker, root, files } = initial;

    let mut file_namespaces = HashMap::new();

    for (file_path, file) in &files {
        let file_ns = checker.add_namespace(Some(root));

        file_namespaces.insert(file_path.clone(), file_ns);
        for ast::Struct { name, loc, .. } in file.iter_structs() {
            if checker.namespaces[file_ns].structs.contains_key(name) {
                let prev_key = checker.namespaces[file_ns].structs[name];
                checker.push_error(TypeCheckError::StructDuplicated(name.clone(), *loc, checker.hir.structs[prev_key].loc));
            }

            checker.add_struct(file_ns, Struct { name: name.clone(), type_params: Vec::new(), super_struct: None, fields: IndexMap::new(), impls: Vec::new(), loc: *loc });
        }
    }

    CollectedStructs { checker, file_namespaces, files, root }
}
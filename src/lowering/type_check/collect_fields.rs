use std::collections::HashMap;
use std::path::PathBuf;
use slotmap::SecondaryMap;
use crate::parsing::ast;
use crate::lowering::type_check::{TypeCheck, NamespaceKey, TypeCheckError, resolve_type};
use crate::lowering::type_check::collect_structs::CollectedStructs;
use crate::lowering::hir::*;

pub(super) struct CollectedFields<'a> {
    pub(super) checker: TypeCheck<'a>,

    pub root: NamespaceKey,
    pub files: HashMap<PathBuf, ast::File<'a>>,
    pub file_namespaces: HashMap<PathBuf, NamespaceKey>,
    pub struct_namespaces: SecondaryMap<StructKey, NamespaceKey>,
}

pub(super) fn collect_fields(collected: CollectedStructs) -> CollectedFields {
    // todo also collect trait required methods here
    let CollectedStructs {
        mut checker, files, root,
        file_namespaces
    } = collected;

    let mut struct_namespaces = SecondaryMap::new();

    for (file_path, file) in &files {
        let file_ns = file_namespaces[file_path];
        for ast::Struct { name, type_params, items, super_struct, .. } in file.iter_structs() {
            let key = checker.get_struct_key(file_ns, name);

            if let Some((super_name, loc)) = super_struct {
                if let Some(super_key) = checker.namespaces[file_ns].structs.get(super_name) {
                    checker.hir.structs[key].super_struct = Some((*super_key, vec![], *loc));
                } else {
                    checker.push_error(TypeCheckError::CouldNotResolveName(super_name.clone(), *loc));
                }
            }

            let struct_ns = checker.add_namespace(Some(file_ns));

            for type_param in type_params {
                let id = checker.add_type_param();
                let typ = Type::TypeParameter { name: type_param.name.clone(), id, bound: None };
                checker.add_type(struct_ns, type_param.name.clone(), typ);
                checker.hir.structs[key].type_params.push(TypeParameter { name: type_param.name.clone(), id, bound: None });
            }
            struct_namespaces.insert(key, struct_ns);

            for item in items {
                match item {
                    ast::StructItem::Field { name: field_name, typ, loc } => {
                        let resolved = resolve_type(&checker, struct_ns, typ);
                        if let Some(prev) = checker.hir.structs[key].fields.get(field_name) {
                            checker.push_error(TypeCheckError::FieldDuplicated(field_name.clone(), name.clone(), *loc, prev.loc));
                        } else {
                            checker.hir.structs[key].fields.insert(field_name.clone(), StructField { name: field_name.clone(), typ: resolved, loc: *loc });
                        }
                    }
                    ast::StructItem::Impl(_) => { }
                }
            }
        }
    };

    CollectedFields { checker, file_namespaces, struct_namespaces, files, root }
}
use std::collections::HashMap;
use indexmap::IndexMap;
use crate::parsing::ast;
use crate::lowering::type_check as tc;
use crate::util::KeyMap;

pub(super) struct StructInfo<'a, 'b> {
    pub name: String,
    pub struct_ns: tc::NamespaceKey,
    pub ast_struct: &'b ast::Struct<'a>,
    pub type_parameters: IndexMap<String, tc::TypeParameterKey>
}

pub(super) struct TraitInfo<'a, 'b> {
    pub name: String,
    pub trait_ns: tc::NamespaceKey,
    pub ast_trait: &'b ast::Trait<'a>,
    pub type_parameters: IndexMap<String, tc::TypeParameterKey>
}

pub(super) struct CollectedTypes<'a, 'b> {
    pub file_info: Vec<tc::collect_struct_prototypes::FileInfo<'a, 'b>>,
    pub structs: KeyMap<tc::StructKey, StructInfo<'a, 'b>>,
    pub traits: KeyMap<tc::TraitKey, TraitInfo<'a, 'b>>
}

impl CollectedTypes<'_, '_> {
    pub fn create_struct_subs(&self, tc::StructType(struct_, variant): tc::StructType) -> HashMap<tc::TypeParameterKey, tc::Type> {
        self.structs[struct_].type_parameters.values().copied().zip(variant).collect()
    }
    
    pub fn create_trait_subs(&self, tc::TraitType(trait_, variant): tc::TraitType) -> HashMap<tc::TypeParameterKey, tc::Type> {
        self.traits[trait_].type_parameters.values().copied().zip(variant).collect()
    }
}

pub(super) fn collect_structs<'a, 'b>(_checker: &mut tc::TypeCheck<'a>, collected: tc::CollectedStructPrototypes<'a, 'b>) -> CollectedTypes<'a, 'b> {
    let tc::CollectedStructPrototypes { structs, traits, file_info, .. } = collected;

    let structs = structs.map(|_, struct_info| {
        StructInfo {
            name: struct_info.name,
            struct_ns: struct_info.struct_ns,
            ast_struct: struct_info.ast_struct,
            type_parameters: struct_info.type_parameters
        }
    });

    let traits = traits.map(|_, trait_info| {
        TraitInfo {
            name: trait_info.name,
            trait_ns: trait_info.trait_ns,
            ast_trait: trait_info.ast_trait,
            type_parameters: trait_info.type_parameters
        }
    });

    CollectedTypes { file_info, structs, traits }
}
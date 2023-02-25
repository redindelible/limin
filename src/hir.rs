use std::collections::HashMap;
use slotmap::{SlotMap, new_key_type};
use crate::source::Location;

new_key_type! {
    pub struct NameKey;
    pub struct TypeKey;
}

pub struct NameDecl<'s> {
    pub name: String,
    pub typ: Type,
    pub decl: Location<'s>
}

pub struct TypeDecl<'s> {
    pub name: String,
    pub typ: Type,
    pub decl: Location<'s>
}

pub enum Type {
    Integer { bits: u8 }
}

pub struct HIR<'s> {
    pub names: SlotMap<NameKey, NameDecl<'s>>,
    pub types: SlotMap<TypeKey, TypeDecl<'s>>
}

pub struct Function<'ir> {
    pub name: String,
    pub body: Expr<'ir>
}

pub enum Expr<'ir> {
    Name { decl: &'ir NameDecl<'ir> }
}
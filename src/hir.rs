use std::collections::HashMap;
use slotmap::{SlotMap, new_key_type};
use crate::source::Location;

new_key_type! {
    pub struct NameKey;
    pub struct TypeKey;
    pub struct StructKey;
}

pub struct NameDecl<'s> {
    pub name: String,
    pub typ: Type,
    pub decl: Location<'s>
}

#[derive(Copy, Clone)]
pub enum Type {
    Integer { bits: u8 },
    Struct { struct_: StructKey }
}

pub struct HIR<'s> {
    pub names: SlotMap<NameKey, NameDecl<'s>>,

    pub structs: SlotMap<StructKey, Struct<'s>>,
}

pub struct Struct<'ir> {
    pub name: String,
    pub fields: HashMap<String, StructField<'ir>>
}

pub struct StructField<'ir> {
    pub name: String,
    pub typ: Type,
    pub loc: Location<'ir>
}

pub struct Function<'ir> {
    pub name: String,
    pub body: Expr<'ir>
}

pub enum Expr<'ir> {
    Name { decl: &'ir NameDecl<'ir> }
}
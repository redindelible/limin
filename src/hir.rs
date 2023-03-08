use std::collections::HashMap;
use slotmap::{SlotMap, new_key_type, SecondaryMap};
use crate::source::Location;

new_key_type! {
    pub struct NameKey;
    pub struct TypeKey;
    pub struct StructKey;
    pub struct FunctionKey;
}

pub enum NameInfo {
    Function { func: FunctionKey }
}

#[derive(Clone)]
pub enum Type {
    Unit,
    Integer { bits: u8 },
    Struct { struct_: StructKey },
    Function { params: Vec<Box<Type>>, ret: Box<Type> }
}

pub struct HIR<'s> {
    pub names: SlotMap<NameKey, NameInfo>,
    pub structs: SlotMap<StructKey, Struct<'s>>,
    pub function_prototypes: SlotMap<FunctionKey, FunctionPrototype<'s>>,
    pub function_bodies: SecondaryMap<FunctionKey, FunctionBody<'s>>
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

pub struct FunctionPrototype<'ir> {
    pub name: String,
    pub params: Vec<Parameter<'ir>>,
    pub ret: Type,

    pub sig: Type
}

pub struct FunctionBody<'ir> {
    pub body: Expr<'ir>
}

pub struct Parameter<'ir> {
    pub name: String,
    pub typ: Type,
    pub loc: Location<'ir>
}

pub enum Expr<'ir> {
    Name { decl: NameKey, loc: Location<'ir> },
    Integer { num: u64, loc: Location<'ir> },
    Errored { loc: Location<'ir> }
}
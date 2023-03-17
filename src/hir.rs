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

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Unit,
    Boolean,
    Errored,
    Integer { bits: u8 },
    Struct { struct_: StructKey },
    Function { params: Vec<Box<Type>>, ret: Box<Type> }
}

#[derive(Default)]
pub struct HIR<'s> {
    pub names: SlotMap<NameKey, NameInfo>,
    pub structs: SlotMap<StructKey, Struct<'s>>,
    pub function_prototypes: SlotMap<FunctionKey, FunctionPrototype<'s>>,
    pub function_bodies: SecondaryMap<FunctionKey, FunctionBody<'s>>
}

impl<'s> HIR<'s> {
    pub fn add_struct(&mut self, struct_: Struct<'s>) -> StructKey {
        self.structs.insert(struct_)
    }

    pub fn get_struct(&mut self, struct_: StructKey) -> &mut Struct<'s> {
        &mut self.structs[struct_]
    }

    pub fn add_prototype(&mut self, proto: FunctionPrototype<'s>) -> FunctionKey {
        self.function_prototypes.insert(proto)
    }

    pub fn add_name(&mut self, name: NameInfo) -> NameKey {
        self.names.insert(name)
    }

    pub fn get_name(&mut self, name: NameKey) -> &mut NameInfo {
        &mut self.names[name]
    }

    pub fn type_of_name(&self, name: NameKey) -> Type {
        match &self.names[name] {
            NameInfo::Function { func } => {
                self.function_prototypes[*func].sig.clone()
            }
        }
    }
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

pub enum LogicOp {
    LessThan,
    GreaterThan
}

pub enum Expr<'ir> {
    Name { decl: NameKey, loc: Location<'ir> },
    Integer { num: u64, loc: Location<'ir> },
    LogicBinOp { left: Box<Expr<'ir>>, op: LogicOp, right: Box<Expr<'ir>> },
    Errored { loc: Location<'ir> }
}

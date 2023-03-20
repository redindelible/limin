use std::collections::HashMap;
use slotmap::{SlotMap, new_key_type, SecondaryMap};
use crate::source::Location;

new_key_type! {
    pub struct NameKey;
    pub struct StructKey;
    pub struct FunctionKey;
}

pub enum NameInfo<'ir> {
    Function { func: FunctionKey },
    Local { containing: FunctionKey, typ: Type, loc: Location<'ir> }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Unit,
    Boolean,
    Errored,
    Integer { bits: u8 },
    Struct { struct_: StructKey },
    Function { params: Vec<Type>, ret: Box<Type> }
}


#[derive(Default)]
pub struct HIR<'s> {
    pub names: SlotMap<NameKey, NameInfo<'s>>,
    pub structs: SlotMap<StructKey, Struct<'s>>,
    pub function_prototypes: SlotMap<FunctionKey, FunctionPrototype<'s>>,
    pub function_bodies: SecondaryMap<FunctionKey, FunctionBody<'s>>
}

impl<'s> HIR<'s> {
    pub fn type_of_name(&self, name: NameKey) -> Type {
        match &self.names[name] {
            NameInfo::Function { func } => self.function_prototypes[*func].sig.clone(),
            NameInfo::Local { typ, .. } => typ.clone()
        }
    }

    pub fn type_of_expr(&self, expr: &Expr<'s>) -> Type {
        match expr {
            Expr::Name { decl, .. } => self.type_of_name(*decl),
            Expr::Integer { .. } => Type::Integer { bits: 32 },
            Expr::Unit { .. } => Type::Unit,
            Expr::LogicBinOp { .. } => Type::Boolean,
            Expr::Block { trailing_expr, .. } => self.type_of_expr(trailing_expr),
            Expr::Errored { .. } => Type::Errored
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

#[derive(Clone)]
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
    Unit { loc: Location<'ir> },
    LogicBinOp { left: Box<Expr<'ir>>, op: LogicOp, right: Box<Expr<'ir>>, loc: Location<'ir> },
    Block { stmts: Vec<Stmt<'ir>>, trailing_expr: Box<Expr<'ir>>, loc: Location<'ir> },
    Errored { loc: Location<'ir> }
}

pub enum Stmt<'ir> {
    Decl { decl: NameKey, value: Expr<'ir>, loc: Location<'ir> },
    Return { value: Expr<'ir>, loc: Location<'ir> },
    Expr { expr: Expr<'ir>, loc: Location<'ir> }
}

use std::fmt::Debug;
use indexmap::IndexMap;
use slotmap::{SlotMap, new_key_type, SecondaryMap};
use crate::source::Location;

new_key_type! {
    pub struct NameKey;
    pub struct StructKey;
    pub struct FunctionKey;
}

pub enum NameInfo<'ir> {
    Function { func: FunctionKey },
    Local { typ: Type, loc: Location<'ir> }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Errored,
    Unit,
    Never,
    Boolean,
    Integer { bits: u8 },
    Struct { struct_: StructKey },
    Function { params: Vec<Type>, ret: Box<Type> }
}

pub struct HIR<'s> {
    pub name: String,
    pub main_function: Option<FunctionKey>,

    pub names: SlotMap<NameKey, NameInfo<'s>>,
    pub structs: SlotMap<StructKey, Struct<'s>>,
    pub function_prototypes: SlotMap<FunctionKey, FunctionPrototype<'s>>,
    pub function_bodies: SecondaryMap<FunctionKey, FunctionBody<'s>>
}

impl<'s> HIR<'s> {
    pub fn new(name: String) -> HIR<'s> {
        HIR {
            name,
            names: SlotMap::with_key(),
            main_function: None,
            structs: SlotMap::with_key(),
            function_prototypes: SlotMap::with_key(),
            function_bodies: SecondaryMap::new()
        }
    }
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
            Expr::Block { trailing_expr, .. } => {
                match trailing_expr {
                    Some(t) => self.type_of_expr(t),
                    None => Type::Unit
                }
            },
            Expr::Call { callee, .. } => {
                let Type::Function { ret, .. } = self.type_of_expr(callee) else {
                    panic!()
                };
                *ret
            },
            Expr::New { struct_, .. } => {
                Type::Struct { struct_: *struct_ }
            },
            Expr::Errored { .. } => Type::Errored
        }
    }

    pub fn is_subtype(&self, this: &Type, of: &Type) -> bool {
        match (this, of) {
            (Type::Errored, _) | (_, Type::Errored) => panic!("found errored type {:?} {:?}", this, of),
            (Type::Never, _) => true,
            (Type::Unit, Type::Unit) => true,
            (Type::Boolean, Type::Boolean) => true,
            (Type::Integer { bits: a }, Type::Integer { bits: b}) if a <= b => true,
            (Type::Struct { struct_: a}, Type::Struct { struct_: b}) if a == b => true,
            (Type::Function { params: a_params, ret: a_ret }, Type::Function { params: b_params, ret: b_ret }) => {
                self.is_subtype(a_ret, b_ret) && b_params.iter().zip(a_params.iter()).all(|(b_typ, a_typ)| self.is_subtype(b_typ, a_typ))
            },
            _ => false
        }
    }
}

pub struct Struct<'ir> {
    pub name: String,
    pub fields: IndexMap<String, StructField<'ir>>,
    pub loc: Location<'ir>
}

#[derive(Clone)]
pub struct StructField<'ir> {
    pub name: String,
    pub typ: Type,
    pub loc: Location<'ir>
}

pub struct FunctionPrototype<'ir> {
    pub name: String,
    pub decl: NameKey,
    pub params: Vec<Parameter<'ir>>,
    pub ret: Type,

    pub sig: Type
}

pub struct FunctionBody<'ir> {
    pub body: Expr<'ir>,
    pub declared: Vec<NameKey>
}

#[derive(Clone)]
pub struct Parameter<'ir> {
    pub name: String,
    pub typ: Type,
    pub loc: Location<'ir>,
    pub decl: NameKey,
}

#[derive(Debug)]
pub enum LogicOp {
    LessThan,
    GreaterThan
}

pub trait MayBreak {
    fn does_break(&self) -> bool;
}

#[derive(Debug)]
pub enum Expr<'ir> {
    Name { decl: NameKey, loc: Location<'ir> },
    Integer { num: u64, loc: Location<'ir> },
    Unit { loc: Location<'ir> },
    LogicBinOp { left: Box<Expr<'ir>>, op: LogicOp, right: Box<Expr<'ir>>, loc: Location<'ir> },
    Block { stmts: Vec<Stmt<'ir>>, trailing_expr: Option<Box<Expr<'ir>>>, declared: Vec<NameKey>, loc: Location<'ir> },
    Call { callee: Box<Expr<'ir>>, arguments: Vec<Expr<'ir>>, loc: Location<'ir> },
    Errored { loc: Location<'ir> },
    New { struct_: StructKey, fields: IndexMap<String, Box<Expr<'ir>>>, loc: Location<'ir> }
}

impl MayBreak for Expr<'_> {
    fn does_break(&self) -> bool {
        match self {
            Expr::Name { .. } | Expr::Integer { .. } | Expr::Unit { .. } | Expr::Errored { .. } => false,
            Expr::LogicBinOp { left, right, .. } => left.does_break() || right.does_break(),
            Expr::Block { stmts, trailing_expr, .. } => {
                stmts.iter().any(|stmt| stmt.does_break()) || trailing_expr.as_ref().map_or(false, |e| e.does_break())
            },
            Expr::Call { callee, arguments, .. } => {
                callee.does_break() || arguments.iter().any(|arg| arg.does_break())
            },
            Expr::New { fields, .. } => {
                fields.values().any(|val| val.does_break())
            } 
        }
    }
}

#[derive(Debug)]
pub enum Stmt<'ir> {
    Decl { decl: NameKey, value: Expr<'ir>, loc: Location<'ir> },
    Return { value: Expr<'ir>, loc: Location<'ir> },
    Expr { expr: Expr<'ir>, loc: Location<'ir> }
}

impl MayBreak for Stmt<'_> {
    fn does_break(&self) -> bool {
        match self {
            Stmt::Decl { value, .. } => value.does_break(),
            Stmt::Expr { expr, .. } => expr.does_break(),
            Stmt::Return { .. } => true
        }
    }
}
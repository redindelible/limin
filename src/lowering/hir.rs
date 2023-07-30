use std::collections::HashMap;
use std::fmt::Debug;
use indexmap::IndexMap;
use slotmap::{SlotMap, new_key_type, SecondaryMap};
use crate::source::Location;

new_key_type! {
    pub struct NameKey;
    pub struct StructKey;
    pub struct FunctionKey;
    pub struct TypeParamKey;
}

pub enum NameInfo<'ir> {
    Function { func: FunctionKey },
    Local { typ: Type, loc: Location<'ir> }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Type {
    Errored,
    Unit,
    Never,
    Boolean,
    Integer { bits: u8 },
    Struct { struct_: StructKey, variant: Vec<Type> },
    Function { params: Vec<Type>, ret: Box<Type> },
    GenericFunction { func: FunctionKey, type_params: Vec<TypeParameter>, params: Vec<Type>, ret: Box<Type> },
    TypeParameter { name: String, bound: Option<Box<Type>>, id: u64 },
    TypeParameterInstance { name: String, id: TypeParamKey }
}

impl Type {
    pub fn subs(&self, map: &HashMap<u64, Type>) -> Type {
        match self {
            Type::Never | Type::Unit | Type::Boolean => self.clone(),
            Type::Errored => Type::Errored,
            Type::Integer { bits } => Type::Integer { bits: *bits },
            Type::TypeParameter { id, .. } if map.contains_key(id) => {
                map[id].clone()
            },
            Type::TypeParameter { .. } | Type::TypeParameterInstance { .. }=> self.clone(),
            Type::Function { params, ret } => {
                Type::Function { params: params.iter().map(|t| t.subs(map)).collect(), ret: Box::new(ret.subs(map)) }
            },
            Type::Struct { struct_, variant } => {
                Type::Struct { struct_: *struct_, variant: variant.iter().map(|t| t.subs(map)).collect() }
            },
            // I don't think this one is possible
            Type::GenericFunction { .. } => panic!(),
        }
    }
}

pub struct HIR<'s> {
    pub name: String,
    pub main_function: Option<FunctionKey>,
    
    // todo find some way to preserve insertion order

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
            Expr::Bool { .. } => Type::Boolean,
            Expr::Unit { .. } => Type::Unit,
            Expr::Block(Block { trailing_expr, .. }) => {
                match trailing_expr {
                    Some(t) => self.type_of_expr(t),
                    None => Type::Unit
                }
            },
            Expr::Call { callee, .. } => {
                match self.type_of_expr(callee) {
                    Type::GenericFunction { ret, .. } | Type::Function { ret, ..} => ret.as_ref().clone(),
                    _ => panic!()
                }
            },
            Expr::GenericCall { generic, callee, .. } => {
                let proto = &self.function_prototypes[*callee];
                let mut map = HashMap::new();
                for (type_param, typ) in proto.type_params.iter().zip(generic.iter()) {
                    map.insert(type_param.id, typ.clone());
                };
                proto.ret.subs(&map)
            },
            Expr::New { struct_, variant, .. } => {
                Type::Struct { struct_: *struct_, variant: variant.clone() }
            },
            Expr::Errored { .. } => Type::Errored,
            Expr::GetAttr { obj, attr, .. } => {
                let Type::Struct { struct_, variant } = self.type_of_expr(obj) else { panic!() };
                let mut map = HashMap::new();
                for (type_param, typ) in self.structs[struct_].type_params.iter().zip(variant.iter()) {
                    map.insert(type_param.id, typ.clone());
                };
                self.structs[struct_].fields[attr].typ.subs(&map)
            }
        }
    }

    pub fn is_subtype(&self, this: &Type, of: &Type) -> bool {
        match (this, of) {
            (Type::Errored, _) | (_, Type::Errored) => panic!("found errored type {:?} {:?}", this, of),
            (Type::Never, _) => true,
            (Type::Unit, Type::Unit) => true,
            (Type::Boolean, Type::Boolean) => true,
            (Type::Integer { bits: a }, Type::Integer { bits: b}) if a <= b => true,
            (Type::Struct { struct_: a, variant: a_var}, Type::Struct { struct_: b, variant: b_var}) if a == b && a_var == b_var => true,
            (Type::Function { params: a_params, ret: a_ret }, Type::Function { params: b_params, ret: b_ret }) => {
                self.is_subtype(a_ret, b_ret) && b_params.iter().zip(a_params.iter()).all(|(b_typ, a_typ)| self.is_subtype(b_typ, a_typ))
            },
            (Type::TypeParameter { id: a, ..}, Type::TypeParameter { id: b, .. }) => a == b,
            _ => false
        }
    }
}

pub struct Struct<'ir> {
    pub name: String,
    pub type_params: Vec<TypeParameter>,
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
    pub type_params: Vec<TypeParameter>,
    pub params: Vec<Parameter<'ir>>,
    pub ret: Type,

    pub sig: Type,
    pub loc: Location<'ir>
}

pub struct FunctionBody<'ir> {
    pub body: Block<'ir>,
    // pub declared: HashMap<String, NameKey>
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct TypeParameter {
    pub name: String,
    pub bound: Option<Type>,
    pub id: u64,
}

impl TypeParameter {
    pub fn as_type(&self) -> Type {
        Type::TypeParameter { name: self.name.clone(), id: self.id, bound: self.bound.as_ref().map(|t| Box::new(t.clone())) }
    }
}

#[derive(Clone)]
pub struct Parameter<'ir> {
    pub name: String,
    pub typ: Type,
    pub loc: Location<'ir>,
    pub decl: NameKey,
}

pub trait MayBreak {
    fn does_break(&self) -> bool;
}

#[derive(Debug)]
pub struct Block<'ir> {
    pub stmts: Vec<Stmt<'ir>>,
    pub trailing_expr: Option<Box<Expr<'ir>>>,
    pub declared: HashMap<String, NameKey>,
    pub loc: Location<'ir>
}

#[derive(Debug)]
pub enum Expr<'ir> {
    Name { decl: NameKey, loc: Location<'ir> },
    Integer { num: u64, loc: Location<'ir> },
    Bool { value: bool, loc: Location<'ir> },
    Unit { loc: Location<'ir> },
    Block(Block<'ir>),
    GetAttr { obj: Box<Expr<'ir>>, attr: String, loc: Location<'ir> },
    Call { callee: Box<Expr<'ir>>, arguments: Vec<Expr<'ir>>, loc: Location<'ir> },
    GenericCall { generic: Vec<Type>, callee: FunctionKey, arguments: Vec<Expr<'ir>>, loc: Location<'ir>},
    Errored { loc: Location<'ir> },
    New { struct_: StructKey, variant: Vec<Type>, fields: IndexMap<String, Box<Expr<'ir>>>, loc: Location<'ir> }
}

impl MayBreak for Expr<'_> {
    fn does_break(&self) -> bool {
        match self {
            Expr::Name { .. } | Expr::Integer { .. } | Expr::Unit { .. } | Expr::Errored { .. } | Expr::Bool { .. } => false,
            Expr::Block( Block { stmts, trailing_expr, .. } ) => {
                stmts.iter().any(|stmt| stmt.does_break()) || trailing_expr.as_ref().map_or(false, |e| e.does_break())
            },
            Expr::Call { callee, arguments, .. } => {
                callee.does_break() || arguments.iter().any(|arg| arg.does_break())
            },
            Expr::GenericCall { arguments, .. } => {
                arguments.iter().any(|arg| arg.does_break())
            },
            Expr::New { fields, .. } => {
                fields.values().any(|val| val.does_break())
            }
            Expr::GetAttr { obj, .. } => {
                obj.does_break()
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
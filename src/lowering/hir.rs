use std::collections::HashMap;
use std::fmt::Debug;
use indexmap::IndexMap;
use crate::source::Location;
use crate::util::{KeyMap, declare_key_type};


declare_key_type! {
    pub struct NameKey;
    pub struct StructKey;
    pub struct TraitKey;
    pub struct FunctionKey;
    pub struct MethodKey;
    pub struct ImplKey;
    pub struct TypeParameterKey;
    pub struct InferenceVariableKey;
}

pub enum NameInfo<'a> {
    Local {
        name: String,
        ty: Type,
        level: usize,
        loc: Location<'a>
    },
    Function {
        name: String,
        key: FunctionKey,
        type_params: Vec<TypeParameterKey>,
        unsub_ty: FunctionType,
        loc: Location<'a>
    }
}

impl<'a> NameInfo<'a> {
    pub fn name(&self) -> &String {
        match self {
            NameInfo::Local { name, .. } => name,
            NameInfo::Function { name, .. } => name
        }
    }
    
    pub fn unsub_ty(&self) -> Type {
        match self {
            NameInfo::Local { ty, .. } => ty.clone(),
            NameInfo::Function { unsub_ty, .. } => unsub_ty.clone().into()
        }
    }

    pub fn loc(&self) -> Location<'a> {
        match self {
            NameInfo::Local { loc, .. } => *loc,
            NameInfo::Function { loc, .. } => *loc
        }
    }
}

pub struct TypeParameterInfo<'a> {
    pub name: String,
    pub loc: Location<'a>
}

pub struct InferenceVariableInfo<'a> {
    pub ty: Option<Type>,
    pub name: String,
    pub loc: Location<'a>
}


#[derive(Clone, Debug)]
pub enum Type {
    Errored,
    Unit,
    Never,
    Boolean,
    SignedInteger(u8),
    UnsignedInteger(u8),
    Gc(Box<Type>),
    Ref(Box<Type>),
    Struct(StructType),
    Trait(TraitType),
    Function(FunctionType),
    TypeParameter(TypeParameterKey),
    InferenceVariable(InferenceVariableKey)
}

impl Type {
    pub fn uses_type_parameter(&self, key: TypeParameterKey) -> bool {
        match self {
            Type::Gc(inner) => inner.uses_type_parameter(key),
            Type::Ref(inner) => inner.uses_type_parameter(key),
            Type::Struct(StructType(_, variant)) => variant.iter().any(|ty| ty.uses_type_parameter(key)),
            Type::Trait(TraitType(_, variant)) => variant.iter().any(|ty| ty.uses_type_parameter(key)),
            Type::Function(FunctionType(params, ret)) => ret.uses_type_parameter(key) || params.iter().any(|ty| ty.uses_type_parameter(key)),
            Type::TypeParameter(ty_key) => *ty_key == key,
            _ => false
        }
    }
}

#[derive(Clone, Debug)]
pub struct StructType(pub StructKey, pub Vec<Type>);

#[derive(Clone, Debug)]
pub struct TraitType(pub TraitKey, pub Vec<Type>);

#[derive(Clone, Debug)]
pub struct FunctionType(pub Vec<Type>, pub Box<Type>);

impl From<StructType> for Type {
    fn from(value: StructType) -> Self {
        Type::Struct(value)
    }
}

impl From<TraitType> for Type {
    fn from(value: TraitType) -> Self {
        Type::Trait(value)
    }
}

impl From<FunctionType> for Type {
    fn from(value: FunctionType) -> Self {
        Type::Function(value)
    }
}

impl From<TypeParameterKey> for Type {
    fn from(value: TypeParameterKey) -> Self {
        Type::TypeParameter(value)
    }
}

pub struct HIR<'a> {
    pub name: String,
    pub main_function: FunctionKey,

    pub names: KeyMap<NameKey, NameInfo<'a>>,
    pub type_parameters: KeyMap<TypeParameterKey, TypeParameterInfo<'a>>,
    pub inference_variables: KeyMap<InferenceVariableKey, InferenceVariableInfo<'a>>,

    pub structs: KeyMap<StructKey, Struct<'a>>,
    pub traits: KeyMap<TraitKey, Trait<'a>>,
    pub functions: KeyMap<FunctionKey, Function<'a>>,
    pub methods: KeyMap<MethodKey, Method<'a>>,
    pub impls: KeyMap<ImplKey, Impl<'a>>
}

pub struct Trait<'ir> {
    pub name: String,
    pub type_params: Vec<TypeParameterKey>,
    pub methods: IndexMap<String, MethodPrototype<'ir>>
}

pub struct MethodPrototype<'ir> {
    pub has_self: bool,
    pub params: Vec<Type>,
    pub ret: Type,
    pub loc: Location<'ir>
}

pub struct Struct<'ir> {
    pub name: String,
    pub type_params: Vec<TypeParameterKey>,
    pub super_struct: Option<(StructType, Location<'ir>)>,
    pub fields: IndexMap<String, StructField<'ir>>,
    pub loc: Location<'ir>
}

pub struct Impl<'ir> {
    pub type_params: Vec<TypeParameterKey>,
    pub impl_trait: Option<()>,
    pub bounds: Vec<()>,
    pub for_type: Type,

    pub methods: HashMap<String, MethodKey>,

    pub loc: Location<'ir>
}

pub struct Method<'ir> {
    pub in_impl: ImplKey,

    pub name: String,
    pub type_params: Vec<TypeParameterKey>,
    pub maybe_self: Option<NameKey>,
    pub params: Vec<Parameter<'ir>>,
    pub ret: Type,

    pub body: Block<'ir>,

    pub loc: Location<'ir>
}

#[derive(Clone)]
pub struct StructField<'ir> {
    pub name: String,
    pub typ: Type,
    pub loc: Location<'ir>
}

pub struct Function<'ir> {
    pub name: String,
    pub decl: NameKey,
    pub type_params: Vec<TypeParameterKey>,
    pub params: Vec<Parameter<'ir>>,
    pub ret: Type,

    pub body: Block<'ir>,

    pub loc: Location<'ir>
}

#[derive(Clone, Debug)]
pub struct Parameter<'ir> {
    pub name: String,
    pub decl: NameKey,
    pub typ: Type,
    pub loc: Location<'ir>,
}

#[derive(Debug)]
pub struct Block<'ir> {
    pub stmts: Vec<Stmt<'ir>>,
    pub trailing_expr: Box<Expr<'ir>>,
    pub yield_type: Type,
    pub always_diverges: bool,
    pub declared: HashMap<String, NameKey>,
    pub loc: Location<'ir>
}

#[derive(Debug, Copy, Clone)]
pub enum Coercion {
    DerefGc
}

#[derive(Debug)]
pub enum Expr<'ir> {
    Name(NameKey, HashMap<TypeParameterKey, Type>, Location<'ir>),
    Integer(u64, Location<'ir>),
    Bool(bool, Location<'ir>),
    Unit(Location<'ir>),
    Never(Location<'ir>),
    Block(Block<'ir>),
    GetAttr { obj: Box<Expr<'ir>>, coercions: Vec<Coercion>, obj_type: StructType, field_ty: Type, attr: String, loc: Location<'ir> },
    Call { callee: Box<Expr<'ir>>, callee_type: FunctionType, arguments: Vec<Expr<'ir>>, loc: Location<'ir> },
    GenericCall { generic: Vec<Type>, callee: FunctionKey, arguments: Vec<Expr<'ir>>, ret_type: Type, loc: Location<'ir>},
    MethodCall { object: Box<Expr<'ir>>, coercions: Vec<Coercion>, method: MethodKey, impl_key: ImplKey, impl_types: Vec<Type>, arguments: Vec<Expr<'ir>>, loc: Location<'ir> },
    CreateStruct { struct_type: StructType, fields: IndexMap<String, Box<Expr<'ir>>>, loc: Location<'ir> },
    New { value: Box<Expr<'ir>>, loc: Location<'ir> },
    IfElse { cond: Box<Expr<'ir>>, then_do: Box<Expr<'ir>>, else_do: Box<Expr<'ir>>, yield_type: Type, loc: Location<'ir> },
    Closure { parameters: Vec<Parameter<'ir>>, body: Block<'ir>, fn_type: FunctionType, loc: Location<'ir> },

    CoerceFromNever(Box<Expr<'ir>>, Type),
    SignExtend(Box<Expr<'ir>>, u8),
    Errored { loc: Location<'ir> },
}

impl<'a> Expr<'a> {
    pub fn loc(&self) -> Location<'a> {
        match self {
            Expr::Name(_, _, loc) => *loc,
            Expr::Integer(_, loc) => *loc,
            Expr::Bool(_, loc) => *loc,
            Expr::Unit(loc) => *loc,
            Expr::Block(block) => block.loc,
            Expr::GetAttr { loc, .. } => *loc,
            Expr::Call { loc, .. } => *loc,
            Expr::GenericCall { loc, .. } => *loc,
            Expr::MethodCall { loc, .. } => *loc,
            Expr::Errored { loc, .. } => *loc,
            Expr::CreateStruct { loc, .. } => *loc,
            Expr::New { loc, .. } => *loc,
            Expr::IfElse { loc, .. } => *loc,
            Expr::Closure { loc, .. } => *loc,
            Expr::Never(loc) => *loc,
            Expr::CoerceFromNever(expr, _) => expr.loc(),
            Expr::SignExtend(expr, _) => expr.loc(),
        }
    }
}

#[derive(Debug)]
pub enum Stmt<'ir> {
    Decl { decl: NameKey, value: Expr<'ir>, loc: Location<'ir> },
    Return { value: Expr<'ir>, loc: Location<'ir> },
    Expr { expr: Expr<'ir>, loc: Location<'ir> }
}

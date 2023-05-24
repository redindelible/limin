use slotmap::{new_key_type, SecondaryMap, SlotMap};
use indexmap::IndexMap;

new_key_type! {
    pub struct FunctionKey;
    pub struct StructKey;
    pub struct LocalKey;
    pub struct BlockKey;
}


pub struct LIR {
    pub main_fn: FunctionKey,

    pub struct_prototypes: SlotMap<StructKey, StructPrototype>,
    pub struct_bodies: SecondaryMap<StructKey, StructBody>,

    pub function_prototypes: SlotMap<FunctionKey, FunctionPrototype>,
    pub function_bodies: SecondaryMap<FunctionKey, FunctionBody>,

    pub blocks: SlotMap<BlockKey, Block>,
    pub locals: SlotMap<LocalKey, LocalInfo>
}

impl LIR {
    pub fn type_of(&self, expr: &Expr) -> Type {
        match expr {
            Expr::Never => Type::Never,
            Expr::Unit => Type::Unit,
            Expr::Integer(_) => Type::Integer(32),
            Expr::Boolean(_) => Type::Boolean,
            Expr::Parameter(func, index) => self.function_prototypes[*func].params[*index].1.clone(),
            Expr::LoadLocal(local) => self.locals[*local].typ.clone(),
            // Expr::StoreLocal(_, value) => self.type_of(value),
            Expr::LoadFunction(func) => self.function_prototypes[*func].sig(),
            Expr::Block(block) => self.blocks[*block].ret_type.clone(),
            Expr::GetAttr(struct_, _, attr) => {
                self.struct_bodies[*struct_].fields[attr].clone()
            }
            Expr::Call(callee, _) => {
                let Type::Function(_, ret) = self.type_of(callee) else { panic!(); };
                ret.as_ref().clone()
            },
            Expr::New(struct_key, _) => Type::Struct(*struct_key),
        }
    }
}

pub struct StructPrototype {
    pub name: String
}

pub struct StructBody {
    pub fields: IndexMap<String, Type>
}

pub struct FunctionPrototype {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub ret: Type
}

impl FunctionPrototype {
    pub fn sig(&self) -> Type {
        Type::Function(self.params.iter().map(|(_, t)| t.clone()).collect(), Box::new(self.ret.clone()))
    }
}

pub struct FunctionBody {
    pub body: BlockKey
}


#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Type {
    Unit,
    Never,
    Boolean,
    Integer(u8),
    Struct(StructKey),
    Function(Vec<Type>, Box<Type>)
}

#[derive(Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub ret: Box<Expr>,

    pub ret_type: Type,
    pub locals: Vec<LocalKey>
}

#[derive(Clone)]
pub struct LocalInfo {
    pub name: String,
    pub typ: Type,
    pub block: BlockKey
}

#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    Never,
    Integer(u64),
    Boolean(bool),
    Parameter(FunctionKey, usize),
    LoadLocal(LocalKey),
    LoadFunction(FunctionKey),
    // StoreLocal(LocalKey, Box<Expr>),
    GetAttr(StructKey, Box<Expr>, String),
    Call(Box<Expr>, Vec<Expr>),
    New(StructKey, IndexMap<String, Expr>),
    Block(BlockKey)
}

#[derive(Clone)]
pub enum Stmt {
    Expr(Box<Expr>),
    Decl(LocalKey, Box<Expr>),
    Ret(Box<Expr>)
}
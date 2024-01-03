use slotmap::{new_key_type, SecondaryMap, SlotMap};
use indexmap::IndexMap;

new_key_type! {
    pub struct FunctionKey;
    pub struct StructKey;
    pub struct LocalKey;
    pub struct BlockKey;
}


pub struct MIR {
    pub main_fn: FunctionKey,

    pub struct_prototypes: SlotMap<StructKey, StructPrototype>,
    pub struct_bodies: SecondaryMap<StructKey, StructBody>,

    pub function_prototypes: SlotMap<FunctionKey, FunctionPrototype>,
    pub function_bodies: SecondaryMap<FunctionKey, FunctionBody>,

    pub blocks: SlotMap<BlockKey, Block>,
    pub locals: SlotMap<LocalKey, LocalInfo>
}

impl MIR {
    pub fn is_zero_sized(&self, ty: &Type) -> bool {
        self.is_zero_sized_helper(ty, &mut Vec::new())
    }

    fn is_zero_sized_helper(&self, ty: &Type, visited: &mut Vec<StructKey>) -> bool {
        match ty {
            Type::Unit => true,
            Type::Never => false,
            Type::Boolean => false,
            Type::Integer(_) => false,
            Type::Struct(key) => {
                if visited.contains(key) {
                    return true;
                }

                visited.push(*key);
                let result = self.struct_bodies[*key].fields.values().all(|ty| self.is_zero_sized_helper(ty, visited));
                visited.pop();
                result
            }
            Type::Function(_) => false,
        }
    }

    pub fn is_empty(&self, ty: &Type) -> bool {
        self.is_empty_helper(ty, &mut Vec::new())
    }

    fn is_empty_helper(&self, ty: &Type, visited: &mut Vec<StructKey>) -> bool {
        match ty {
            Type::Never => true,
            Type::Struct(key) => {
                if visited.contains(key) {
                    return false;
                }

                visited.push(*key);
                let result = self.struct_bodies[*key].fields.values().any(|ty| self.is_empty(ty));
                visited.pop();
                result
            }
            Type::Function(fn_type) => {
                fn_type.params.iter().any(|ty| self.is_empty_helper(ty, visited))
            }

            Type::Unit => false,
            Type::Boolean => false,
            Type::Integer(_) => false,
        }
    }
}

pub struct StructPrototype {
    pub name: String
}

pub struct StructBody {
    pub super_struct: Option<StructKey>,
    pub fields: IndexMap<String, Type>
}

impl StructBody {
    pub fn all_fields<'a>(&'a self, mir: &'a MIR) -> IndexMap<&'a String, &'a Type> {
        let mut fields = IndexMap::new();
        if let Some(super_) = &self.super_struct {
            fields.extend(mir.struct_bodies[*super_].all_fields(mir));
        }
        fields.extend(self.fields.iter());
        fields
    }
}

pub struct FunctionPrototype {
    pub name: String,
    pub fn_type: FunctionType,
}

impl FunctionPrototype {
    pub fn sig(&self) -> Type {
        Type::Function(self.fn_type.clone())
    }
}

pub struct FunctionBody {
    pub params: Vec<(String, LocalKey)>,
    pub body: BlockKey
}


#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Type {
    Unit,
    Never,
    Boolean,
    Integer(u8),
    Struct(StructKey),
    Function(FunctionType)
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct FunctionType {
    pub params: Vec<Type>,
    pub ret: Box<Type>
}

#[derive(Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub ret: Box<Expr>,

    pub ret_type: Type,
    pub locals: Vec<LocalKey>,
    pub level: usize,
}

#[derive(Clone)]
pub struct LocalInfo {
    pub name: String,
    pub typ: Type,
    pub block: BlockKey,
    pub is_closed: bool,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    Never,
    Integer(u64),
    Boolean(bool),
    LoadLocal(LocalKey),
    LoadFunction(FunctionKey),
    // StoreLocal(LocalKey, Box<Expr>),
    GetAttr(StructKey, Box<Expr>, String),
    Call(FunctionType, Box<Expr>, Vec<Expr>),
    New(StructKey, IndexMap<String, Expr>),
    Block(BlockKey),
    IfElse { cond: Box<Expr>, then_do: Box<Expr>, else_do: Box<Expr>, yield_type: Type },
    Closure { parameters: Vec<ClosureParameter>, fn_type: FunctionType, body: BlockKey, closed_blocks: Vec<BlockKey> },

    CoerceFromNever(Box<Expr>),
    SignExtend(Box<Expr>, u8)
}

#[derive(Debug, Clone)]
pub struct ClosureParameter {
    pub name: String,
    pub key: LocalKey,
    pub typ: Type
}

#[derive(Clone)]
pub enum Stmt {
    Expr(Box<Expr>),
    Decl(LocalKey, Type, Box<Expr>),
    Ret(Box<Expr>)
}
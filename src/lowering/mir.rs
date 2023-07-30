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
            Type::Function(_, _) => false,
        }
    }

    pub fn is_never(&self, ty: &Type) -> bool {
        self.is_never_helper(ty, &mut Vec::new())
    }

    fn is_never_helper(&self, ty: &Type, visited: &mut Vec<StructKey>) -> bool {
        match ty {
            Type::Never => true,
            Type::Struct(key) => {
                if visited.contains(key) {
                    return false;
                }

                visited.push(*key);
                let result = self.struct_bodies[*key].fields.values().any(|ty| self.is_never(ty));
                visited.pop();
                result
            }
            Type::Function(params, _) => {
                params.iter().any(|ty| self.is_never_helper(ty, visited))
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
    Decl(LocalKey, Type, Box<Expr>),
    Ret(Box<Expr>)
}
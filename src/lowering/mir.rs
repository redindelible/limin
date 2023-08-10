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
            Expr::LoadLocal(local) => self.locals[*local].typ.clone(),
            // Expr::StoreLocal(_, value) => self.type_of(value),
            Expr::LoadFunction(func) => self.function_prototypes[*func].sig(),
            Expr::Block(block) => self.blocks[*block].ret_type.clone(),
            Expr::GetAttr(struct_, _, attr) => {
                self.struct_bodies[*struct_].fields[attr].clone()
            }
            Expr::Call(fn_type, _, _) => fn_type.ret.as_ref().clone(),
            Expr::New(struct_key, _) => Type::Struct(*struct_key),
            Expr::IfElse { yield_type, .. } => yield_type.clone(),
            Expr::Closure { fn_type, .. } => Type::Function(fn_type.clone()),
            Expr::Cast { expr, cast_type } => {
                match cast_type {
                    CastType::StructToSuper { to, .. } => Type::Struct(*to)
                }
            }
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
            Type::Function(_) => false,
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
            Type::Function(fn_type) => {
                fn_type.params.iter().any(|ty| self.is_never_helper(ty, visited))
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

    Cast { expr: Box<Expr>, cast_type: CastType }
}

#[derive(Debug, Clone)]
pub enum CastType {
    StructToSuper { from: StructKey, to: StructKey }
}

#[derive(Debug, Clone)]
pub struct ClosureParameter {
    pub name: String,
    pub key: LocalKey,
    pub typ: Type
}

impl Expr {
    pub fn always_diverges(&self, mir: &MIR) -> bool {
        match self {
            Expr::Unit => false,
            Expr::Never => true,
            Expr::Integer(_) => false,
            Expr::Boolean(_) => false,
            // Expr::Parameter(_, _) => false,
            Expr::LoadLocal(_) => false,
            Expr::LoadFunction(_) => false,
            Expr::GetAttr(_, obj, _) => {
                obj.always_diverges(mir)
            }
            Expr::Call(fn_type, callee, args) => {
                if callee.always_diverges(mir) || args.iter().any(|arg| arg.always_diverges(mir)) {
                    return true;
                }

                mir.is_never(&fn_type.ret)
            }
            Expr::New(_, fields) => fields.values().any(|field| field.always_diverges(mir)),
            Expr::Block(block) => {
                let block = &mir.blocks[*block];
                block.stmts.iter().any(|stmt| stmt.always_diverges(mir)) || block.ret.always_diverges(mir)
            }
            Expr::IfElse { cond, then_do, else_do, .. } => {
                cond.always_diverges(mir) || (then_do.always_diverges(mir) && else_do.always_diverges(mir))
            },
            Expr::Closure { .. } => false,
            Expr::Cast { expr, .. } => expr.always_diverges(mir)
        }
    }
}

#[derive(Clone)]
pub enum Stmt {
    Expr(Box<Expr>),
    Decl(LocalKey, Type, Box<Expr>),
    Ret(Box<Expr>)
}

impl Stmt {
    pub fn always_diverges(&self, mir: &MIR) -> bool {
        match self {
            Stmt::Expr(e) => e.always_diverges(mir),
            Stmt::Decl(_, _, value) => value.always_diverges(mir),
            Stmt::Ret(_) => true,
        }
    }
}
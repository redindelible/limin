use indexmap::IndexMap;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct FunctionID(pub(super) usize);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct StructID(pub(super) usize);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct LocalID(pub(super) usize);

pub struct LIR {
    pub(super) functions: IndexMap<FunctionID, Function>,
    pub(super) structs: IndexMap<StructID, Struct>,

    pub(super) main_function: FunctionID
}

pub struct Function {
    pub(super) id: FunctionID,
    pub(super) name: String,
    pub(super) parameters: Vec<FunctionParameter>,
    pub(super) ret: Option<Type>,
    pub(super) block: BlockDiverge,
}

impl Function {
    pub fn signature(&self) -> Type {
        Type::Function(self.parameters.iter().map(|p| p.ty.clone()).collect(), self.ret.clone().map(Box::new))
    }
}

pub(super) struct FunctionParameter {
    pub(super) name: String,
    pub(super) ty: Type,
    pub(super) local: LocalID,
}

pub struct Struct {
    pub(super) id: StructID,
    pub(super) name: String,
    pub(super) fields: Vec<StructField>
}

#[derive(Clone)]
pub(super) struct StructField {
    pub(super) name: String,
    pub(super) ty: Type
}

#[derive(Debug)]
pub enum Instruction {
    DeclareLocal(LocalID),
    LoadLocal(LocalID),
    StoreLocal(LocalID),
    LoadFunction(FunctionID),
    LoadI32(i32),
    LoadBool(bool),
    LoadNull,
    Return,
    ReturnVoid,
    Pop,
    DerefGc,
    BlockValue(BlockValue),
    BlockVoid(BlockVoid),
    BlockDiverge(BlockDiverge),
    CreateTuple(usize),
    GetElement(usize),
    Splat,
    CreateZeroInitGcStruct(StructID),
    CreateStruct(StructID, Vec<String>),
    CreateNew,
    GetField(StructID, String),
    GetGcField(StructID, String),
    SetGcField(StructID, String),
    Call(usize),
    CallVoid(usize),
    IfElseValue { then_do: BlockValueOrDiverge, else_do: BlockValueOrDiverge, ty: Type },
    IfElseVoid { then_do: BlockVoidOrDiverge, else_do: BlockVoidOrDiverge },
    IfElseDiverge { then_do: BlockDiverge, else_do: BlockDiverge },

    StatePoint,
    Unreachable
}

#[derive(Debug)]
pub enum BlockValueOrDiverge {
    Value(BlockValue),
    Diverge(BlockDiverge)
}

impl BlockValueOrDiverge {
    pub fn instructions(&self) -> &[Instruction] {
        match self {
            BlockValueOrDiverge::Value(b) => &b.instructions,
            BlockValueOrDiverge::Diverge(b) => &b.instructions
        }
    }

    pub fn diverges(&self) -> bool {
        match self {
            BlockValueOrDiverge::Value(_) => false,
            BlockValueOrDiverge::Diverge(_) => true
        }
    }
}

impl From<BlockValue> for BlockValueOrDiverge {
    fn from(value: BlockValue) -> Self {
        BlockValueOrDiverge::Value(value)
    }
}

impl From<BlockDiverge> for BlockValueOrDiverge {
    fn from(value: BlockDiverge) -> Self {
        BlockValueOrDiverge::Diverge(value)
    }
}

#[derive(Debug)]
pub enum BlockVoidOrDiverge {
    Void(BlockVoid),
    Diverge(BlockDiverge)
}

impl BlockVoidOrDiverge {
    pub fn instructions(&self) -> &[Instruction] {
        match self {
            BlockVoidOrDiverge::Void(b) => &b.instructions,
            BlockVoidOrDiverge::Diverge(b) => &b.instructions
        }
    }

    pub fn diverges(&self) -> bool {
        match self {
            BlockVoidOrDiverge::Void(_) => false,
            BlockVoidOrDiverge::Diverge(_) => true
        }
    }
}

impl From<BlockVoid> for BlockVoidOrDiverge {
    fn from(value: BlockVoid) -> Self {
        BlockVoidOrDiverge::Void(value)
    }
}

impl From<BlockDiverge> for BlockVoidOrDiverge {
    fn from(value: BlockDiverge) -> Self {
        BlockVoidOrDiverge::Diverge(value)
    }
}

#[derive(Debug)]
pub struct BlockValue {
    pub(super) instructions: Vec<Instruction>,
    pub(super) yield_type: Type
}

#[derive(Debug)]
pub struct BlockVoid {
    pub(super) instructions: Vec<Instruction>
}

#[derive(Debug)]
pub struct BlockDiverge {
    pub(super) instructions: Vec<Instruction>,
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub enum Type {
    AnyGc,
    Boolean,
    Int32,
    Function(Vec<Type>, Option<Box<Type>>),
    Struct(StructID),
    Tuple(Vec<Type>),
    Gc(Box<Type>)
}

impl Type {
    pub fn gc_struct(struct_: StructID) -> Type {
        Type::Gc(Box::new(Type::Struct(struct_)))
    }
}
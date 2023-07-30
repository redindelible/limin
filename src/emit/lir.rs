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
    pub(super) block: Block,
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
    BlockValue(Block),
    BlockVoid(Block),
    CreateTuple(usize),
    GetElement(usize),
    Splat,
    CreateStruct(StructID, Vec<String>),
    GetField(StructID, String),
    Call(usize),
    CallVoid(usize),

    StatePoint,
    Unreachable
}

#[derive(Debug)]
pub struct Block {
    pub(super) instructions: Vec<Instruction>,
    pub(super) yield_type: Option<Type>,
    pub(super) diverges: bool
}

impl Block {
    pub fn new(instructions: Vec<Instruction>, yield_ty: Option<Type>, diverges: bool) -> Block {
        if diverges {
            assert!(yield_ty.is_none());
        }
        Block { instructions, yield_type: yield_ty, diverges }
    }

    pub fn yield_type(&self) -> Option<Type> {
        self.yield_type.clone()
    }
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub enum Type {
    AnyRef,
    Boolean,
    Int32,
    Function(Vec<Type>, Option<Box<Type>>),
    StructRef(StructID),
    Tuple(Vec<Type>)
}
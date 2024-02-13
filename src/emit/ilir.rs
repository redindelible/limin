use std::collections::HashMap;
use crate::emit::lir::{Type, FunctionID, StructID};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Location(pub usize);

pub enum LocationInfo {
    InFrame {
        frame_id: u32,
        idx: usize,
        ty: Type
    },
    Unmanaged {
        id: u32,
        ty: Type
    }
}

pub struct FrameInfo {
    pub(super) id: u32,
    pub(super) types: Vec<Type>
}

pub struct Function {
    pub(super) name: String,
    pub(super) parameters: Vec<FunctionParameter>,
    pub(super) ret: Option<Type>,
    pub(super) block: BlockWithDiverge,
    pub(super) frames: Vec<FrameInfo>,
    pub(super) locations: HashMap<Location, LocationInfo>
}

pub(super) struct FunctionParameter {
    pub(super) name: String,
    pub(super) ty: Type,
    pub(super) location: Location,
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
    DeclareLocal { ty: Type, local: Location, value: Location },
    LoadLocal { local: Location, store: Location },
    StoreLocal { local: Location, value: Location },
    LoadFunction { func: FunctionID, store: Location },
    LoadI32 { value: i32, store: Location },
    LoadBool { value: bool, store: Location },
    LoadNull { store: Location },
    Return { value: Location },
    ReturnVoid,
    BlockValue { block: BlockWithValue, store: Location },
    BlockVoid { block: BlockWithNoValue },
    BlockDiverge { block: BlockWithDiverge },
    DerefGc { pointer: Location, pointee: Type, store: Location },
    CreateTuple { values: Vec<Location>, store: Location },
    GetElement { value: Location, idx: usize, store: Location },
    Splat { value: Location, stores: Vec<Location> },
    CreateNew { object: Location, store: Location },
    CreateZeroInitGcStruct { id: StructID, store: Location },
    CreateStruct { id: StructID, fields: Vec<Location>, store: Location },
    GetField { obj: Location, id: StructID, field: usize, store: Location },
    GetGcField { obj: Location, id: StructID, field: usize, store: Location },
    SetGcField { obj: Location, id: StructID, field: usize, value: Location, store: Location },
    Call { callee: Location, arguments: Vec<Location>, store: Location },
    CallVoid { callee: Location, arguments: Vec<Location> },
    IfElseValue { cond: Location, then_do: BlockValueOrDiverge, else_do: BlockValueOrDiverge, store: Location },
    IfElseVoid { cond: Location, then_do: BlockNoValueOrDiverge, else_do: BlockNoValueOrDiverge },
    IfElseDiverge { cond: Location, then_do: BlockWithDiverge, else_do: BlockWithDiverge },

    StatePoint { id: u32 },
    Unreachable
}

#[derive(Debug)]
pub enum BlockValueOrDiverge {
    Value(BlockWithValue),
    Diverge(BlockWithDiverge)
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

#[derive(Debug)]
pub enum BlockNoValueOrDiverge {
    NoValue(BlockWithNoValue),
    Diverge(BlockWithDiverge)
}

impl BlockNoValueOrDiverge {
    pub fn instructions(&self) -> &[Instruction] {
        match self {
            BlockNoValueOrDiverge::NoValue(b) => &b.instructions,
            BlockNoValueOrDiverge::Diverge(b) => &b.instructions
        }
    }

    pub fn diverges(&self) -> bool {
        match self {
            BlockNoValueOrDiverge::NoValue(_) => false,
            BlockNoValueOrDiverge::Diverge(_) => true
        }
    }
}

#[derive(Debug)]
pub struct BlockWithValue {
    pub(super) instructions: Vec<Instruction>,
    pub(super) yield_type: Type,
    pub(super) yield_loc: Location
}

#[derive(Debug)]
pub struct BlockWithNoValue {
    pub(super) instructions: Vec<Instruction>
}

#[derive(Debug)]
pub struct BlockWithDiverge {
    pub(super) instructions: Vec<Instruction>,
}
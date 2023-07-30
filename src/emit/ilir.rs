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
    pub(super) block: Block,
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
    BlockValue { block: Block, value: Location, store: Location },
    BlockVoid { block: Block },
    CreateTuple { values: Vec<Location>, store: Location },
    GetElement { value: Location, idx: usize, store: Location },
    Splat { value: Location, stores: Vec<Location> },
    CreateStruct { id: StructID, fields: Vec<Location>, store: Location },
    GetField { value: Location, id: StructID, field: usize, store: Location },
    Call { callee: Location, arguments: Vec<Location>, store: Location },
    CallVoid { callee: Location, arguments: Vec<Location> },

    StatePoint { id: u32 },
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
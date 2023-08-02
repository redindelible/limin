use std::collections::HashMap;
use std::iter;
use crate::emit::{ilir, lir};
use crate::util::{StackKey, StackTrace};

pub fn trace_roots(function: lir::FunctionID, lir: &lir::LIR) -> ilir::Function {
    RootTracer::new().trace_fn(function, lir)
}

#[derive(Debug)]
struct StackSlotInfo {
    is_managed: bool,
    ty: lir::Type
}

impl StackSlotInfo {
    fn new_managed(ty: lir::Type) -> StackSlotInfo {
        StackSlotInfo {
            is_managed: true,
            ty
        }
    }

    fn new_unmanaged(ty: lir::Type) -> StackSlotInfo {
        StackSlotInfo {
            is_managed: false,
            ty
        }
    }
}

struct LocalInfo {
    key: StackKey,
    ty: lir::Type
}

struct RootTracer {
    tracer: StackTrace<StackSlotInfo>,
    locals: HashMap<lir::LocalID, LocalInfo>,

    locations: HashMap<StackKey, ilir::Location>,
    locations_map: HashMap<ilir::Location, ilir::LocationInfo>,
    frames: Vec<ilir::FrameInfo>
}

impl RootTracer {
    fn new() -> Self {
        RootTracer {
            tracer: StackTrace::new(),
            locals: HashMap::new(),
            locations: HashMap::new(),
            locations_map: HashMap::new(),
            frames: Vec::new()
        }
    }

    fn is_managed(ty: &lir::Type) -> bool {
        match ty {
            lir::Type::AnyRef => true,
            lir::Type::Boolean => false,
            lir::Type::Int32 => false,
            lir::Type::Function(_, _) => false,
            lir::Type::StructRef(_) => true,
            lir::Type::Tuple(tys) => tys.iter().any(Self::is_managed)
        }
    }

    fn allocate_local(&mut self, ty: lir::Type, id: lir::LocalID) -> StackKey {
        let key = if Self::is_managed(&ty) {
            self.tracer.push(StackSlotInfo::new_managed(ty.clone()))
        } else {
            self.tracer.push(StackSlotInfo::new_unmanaged(ty.clone()))
        };

        self.locals.insert(id, LocalInfo { key, ty });
        key
    }

    fn push(&mut self, ty: lir::Type) -> ilir::Location {
        let key = if Self::is_managed(&ty) {
            self.tracer.push(StackSlotInfo::new_managed(ty.clone()))
        } else {
            self.tracer.push(StackSlotInfo::new_unmanaged(ty.clone()))
        };

        self.stack_loc(key)
    }

    fn pop(&mut self) -> ilir::Location {
        let key = self.tracer.pop();
        self.stack_loc(key)
    }

    fn stack_loc(&mut self, key: StackKey) -> ilir::Location {
        let next = self.locations.len();
        let location = *self.locations.entry(key).or_insert(ilir::Location(next));
        if !self.locations_map.contains_key(&location) {
            let id = self.locations_map.len();
            self.locations_map.insert(location, ilir::LocationInfo::Unmanaged {
                id: id as u32,
                ty: self.tracer.ann(key).ty.clone()
            });
        }
        location
    }

    fn trace_fn(mut self, id: lir::FunctionID, lir: &lir::LIR) -> ilir::Function {
        let func = &lir.functions[&id];

        let mut parameters: Vec<ilir::FunctionParameter> = Vec::new();

        for param in &func.parameters {
            let location = self.allocate_local(param.ty.clone(), param.local);
            parameters.push(ilir::FunctionParameter {
                ty: param.ty.clone(),
                name: param.name.clone(),
                location: self.stack_loc(location)
            })
        }

        let block = self.trace_block_diverge(&func.block, lir);

        ilir::Function {
            name: func.name.clone(),
            parameters,
            ret: func.ret.clone(),
            block,
            frames: self.frames,
            locations: self.locations_map
        }
    }

    fn trace_block_value_or_diverge(&mut self, block: &lir::BlockValueOrDiverge, lir: &lir::LIR) -> ilir::BlockValueOrDiverge {
        let starting = self.tracer.top();

        let instructions = self.trace_instructions(block.instructions(), lir);

        match block {
            lir::BlockValueOrDiverge::Diverge(_) => {
                self.tracer.pop_to(starting);
                ilir::BlockValueOrDiverge::Diverge(ilir::BlockWithDiverge {
                    instructions
                })
            }
            lir::BlockValueOrDiverge::Value(block) => {
                let yield_loc = self.pop();
                self.tracer.pop_to(starting);
                ilir::BlockValueOrDiverge::Value(ilir::BlockWithValue {
                    instructions,
                    yield_type: block.yield_type.clone(),
                    yield_loc
                })
            }
        }
    }

    fn trace_block_diverge(&mut self, block: &lir::BlockDiverge, lir: &lir::LIR) -> ilir::BlockWithDiverge {
        let starting = self.tracer.top();
        let instructions = self.trace_instructions(&block.instructions, lir);
        self.tracer.pop_to(starting);
        ilir::BlockWithDiverge {
            instructions
        }
    }

    fn trace_block_void(&mut self, block: &lir::BlockVoid, lir: &lir::LIR) -> ilir::BlockWithNoValue {
        let starting = self.tracer.top();
        let instructions = self.trace_instructions(&block.instructions, lir);
        self.tracer.pop_to(starting);
        ilir::BlockWithNoValue {
            instructions
        }
    }

    fn trace_block_value(&mut self, block: &lir::BlockValue, lir: &lir::LIR) -> ilir::BlockWithValue {
        let starting = self.tracer.top();
        let instructions = self.trace_instructions(&block.instructions, lir);
        let yield_loc = self.pop();
        self.tracer.pop_to(starting);
        ilir::BlockWithValue {
            instructions,
            yield_type: block.yield_type.clone(),
            yield_loc
        }
    }

    fn trace_block_no_value_or_diverge(&mut self, block: &lir::BlockVoidOrDiverge, lir: &lir::LIR) -> ilir::BlockNoValueOrDiverge {
        let starting = self.tracer.top();

        let instructions = self.trace_instructions(block.instructions(), lir);

        if block.diverges() {
            self.tracer.pop_to(starting);
            ilir::BlockNoValueOrDiverge::Diverge(ilir::BlockWithDiverge {
                instructions
            })
        } else {
            self.tracer.pop_to(starting);
            ilir::BlockNoValueOrDiverge::NoValue(ilir::BlockWithNoValue {
                instructions
            })
        }
    }

    fn trace_instructions(&mut self, lir_instructions: &[lir::Instruction], lir: &lir::LIR) -> Vec<ilir::Instruction> {
        use crate::emit::lir::Instruction;

        let mut instructions: Vec<ilir::Instruction> = Vec::new();

        for instr in lir_instructions {
            match instr {
                Instruction::DeclareLocal(id) => {
                    let value_key = self.tracer.pop();
                    let ty = self.tracer.ann(value_key).ty.clone();
                    let local_key = self.allocate_local(ty.clone(), *id);
                    instructions.push(ilir::Instruction::DeclareLocal {
                        ty,
                        value: self.stack_loc(value_key),
                        local: self.stack_loc(local_key)
                    });
                }
                Instruction::LoadLocal(id) => {
                    instructions.push(ilir::Instruction::LoadLocal {
                        local: self.stack_loc(self.locals[id].key),
                        store: self.push(self.locals[id].ty.clone()),
                    })
                }
                Instruction::StoreLocal(id) => {
                    let key = self.tracer.pop();
                    instructions.push(ilir::Instruction::StoreLocal {
                        value: self.stack_loc(key),
                        local: self.stack_loc(self.locals[id].key)
                    });
                }
                Instruction::LoadFunction(id) => {
                    instructions.push(ilir::Instruction::LoadFunction {
                        func: *id,
                        store: self.push(lir.functions[id].signature())
                    });
                }
                Instruction::LoadI32(value) => {
                    instructions.push(ilir::Instruction::LoadI32 {
                        value: *value,
                        store: self.push(lir::Type::Int32)
                    });
                }
                Instruction::LoadBool(value) => {
                    instructions.push(ilir::Instruction::LoadBool {
                        value: *value,
                        store: self.push(lir::Type::Boolean)
                    });
                }
                Instruction::LoadNull => {
                    instructions.push(ilir::Instruction::LoadNull {
                        store: self.push(lir::Type::AnyRef)
                    });
                }
                Instruction::Return => {
                    instructions.push(ilir::Instruction::Return {
                        value: self.pop()
                    });
                }
                Instruction::ReturnVoid => {
                    instructions.push(ilir::Instruction::ReturnVoid);
                }
                Instruction::Pop => {
                    self.tracer.pop();
                }
                Instruction::BlockValue(child) => {
                    let block = self.trace_block_value(child, lir);
                    instructions.push(ilir::Instruction::BlockValue {
                        block, store: self.push(child.yield_type.clone())
                    })
                }
                Instruction::BlockVoid(child) => {
                    let block = self.trace_block_void(child, lir);
                    instructions.push(ilir::Instruction::BlockVoid {
                        block
                    });
                }
                Instruction::BlockDiverge(child) => {
                    let block = self.trace_block_diverge(child, lir);
                    instructions.push(ilir::Instruction::BlockDiverge {
                        block
                    });
                }
                Instruction::CreateTuple(count) => {
                    let mut values = Vec::with_capacity(*count);
                    let mut tys = Vec::new();

                    for _ in 0..*count {
                        let key = self.tracer.pop();
                        let ty = self.tracer.ann(key).ty.clone();

                        values.insert(0, self.stack_loc(key));
                        tys.insert(0, ty);
                    }

                    instructions.push(ilir::Instruction::CreateTuple {
                        values,
                        store: self.push(lir::Type::Tuple(tys))
                    });
                }
                Instruction::GetElement(_) => {
                    todo!()
                }
                Instruction::Splat => {
                    let key = self.tracer.pop();
                    let ty = self.tracer.ann(key).ty.clone();
                    let lir::Type::Tuple(tys) = ty else { panic!() };

                    let mut stores = Vec::new();
                    for ty in tys {
                        stores.push(self.push(ty));
                    }

                    instructions.push(ilir::Instruction::Splat {
                        value: self.stack_loc(key),
                        stores
                    });
                }
                Instruction::CreateStruct(struct_id, field_arguments) => {
                    let fields_in_order = &lir.structs[struct_id].fields;

                    let field_locations: HashMap<&String, ilir::Location> = field_arguments.iter().map(|arg| (arg, self.pop())).collect();

                    let fields = fields_in_order.iter().map(|name| field_locations[&name.name]).collect();

                    instructions.push(ilir::Instruction::CreateStruct {
                        id: *struct_id,
                        fields,
                        store: self.push(lir::Type::StructRef(*struct_id))
                    });
                }
                Instruction::GetField(struct_id, field_name) => {
                    let value = self.pop();
                    let (idx, found) = lir.structs[struct_id].fields.iter().enumerate().find(|(_, field)| &field.name == field_name).unwrap();
                    instructions.push(ilir::Instruction::GetField {
                        id: *struct_id,
                        value,
                        field: idx,
                        store: self.push(found.ty.clone())
                    });
                }
                Instruction::Call(num_args) => {
                    let mut arguments: Vec<ilir::Location> = iter::from_fn(|| Some(self.pop())).take(*num_args).collect();
                    arguments.reverse();
                    let callee_key = self.tracer.pop();
                    let ret_type = match &self.tracer.ann(callee_key).ty {
                        lir::Type::Function(_, ret) => {
                            ret.as_ref().unwrap().as_ref().clone()
                        }
                        other => panic!("{:?}", other)
                    };
                    let callee = self.stack_loc(callee_key);

                    instructions.push(ilir::Instruction::Call {
                        callee,
                        arguments,
                        store: self.push(ret_type)
                    })
                }
                Instruction::CallVoid(_) => {
                    todo!()
                }
                Instruction::StatePoint => {
                    let id = self.frames.len() as u32;

                    let mut frame_types = Vec::new();
                    let curr = self.tracer.top();
                    for (key, slot) in self.tracer.stack_mut(curr).collect::<Vec<_>>().into_iter().rev() {
                        if slot.is_managed {
                            let loc = {
                                let next = self.locations.len();
                                let location = *self.locations.entry(key).or_insert(ilir::Location(next));
                                if !self.locations_map.contains_key(&location) {
                                    let id = self.locations_map.len();
                                    self.locations_map.insert(location, ilir::LocationInfo::Unmanaged {
                                        id: id as u32,
                                        ty: slot.ty.clone()
                                    });
                                }
                                location
                            };
                            let location_info = self.locations_map.get_mut(&loc).unwrap();
                            if !matches!(location_info, ilir::LocationInfo::InFrame { .. }) {
                                *location_info = ilir::LocationInfo::InFrame {
                                    frame_id: id,
                                    idx: frame_types.len(),
                                    ty: slot.ty.clone()
                                };
                            }
                            frame_types.push(slot.ty.clone());
                        }
                    }

                    self.frames.push(ilir::FrameInfo {
                        id,
                        types: frame_types
                    });

                    instructions.push(ilir::Instruction::StatePoint { id });
                }
                Instruction::IfElseValue { then_do, else_do, ty } => {
                    let cond = self.pop();
                    let then_do = self.trace_block_value_or_diverge(then_do, lir);
                    let else_do = self.trace_block_value_or_diverge(else_do, lir);
                    let store = self.push(ty.clone());
                    instructions.push(ilir::Instruction::IfElseValue { cond, then_do, else_do, store });
                }
                Instruction::IfElseVoid { then_do, else_do } => {
                    let cond = self.pop();
                    let then_do = self.trace_block_no_value_or_diverge(then_do, lir);
                    let else_do = self.trace_block_no_value_or_diverge(else_do, lir);
                    instructions.push(ilir::Instruction::IfElseVoid { cond, then_do, else_do });
                }
                Instruction::IfElseDiverge { then_do, else_do } => {
                    let cond = self.pop();
                    let then_do = self.trace_block_diverge(then_do, lir);
                    let else_do = self.trace_block_diverge(else_do, lir);
                    instructions.push(ilir::Instruction::IfElseDiverge { cond, then_do, else_do });
                }

                Instruction::Unreachable => {}
            }
        };

        instructions
    }
}

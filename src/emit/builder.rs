use std::collections::HashMap;
use indexmap::IndexMap;
use crate::emit::lir;
use crate::util::Counter;


pub struct LIRBuilder {
    functions: IndexMap<lir::FunctionID, Option<lir::Function>>,
    structs: IndexMap<lir::StructID, Option<lir::Struct>>,

    main_function: Option<lir::FunctionID>
}

impl LIRBuilder {
    pub fn new() -> LIRBuilder {
        LIRBuilder {
            functions: IndexMap::new(),
            structs: IndexMap::new(),
            main_function: None
        }
    }

    pub fn finish(self) -> lir::LIR {
        let LIRBuilder { functions, structs, main_function } = self;

        lir::LIR {
            functions: functions.into_iter().map(|(k, v)| (k, v.unwrap())).collect(),
            structs: structs.into_iter().map(|(k, v)| (k, v.unwrap())).collect(),
            main_function: main_function.unwrap(),
        }
    }

    pub fn declare_function(&mut self) -> lir::FunctionID {
        let id = lir::FunctionID(self.functions.len());
        if self.functions.insert(id, None).is_some() {
            panic!("function already declared")
        };
        id
    }

    pub fn define_function<F>(&mut self, id: lir::FunctionID, name: String, f: F) -> lir::Type where F: FnOnce(FunctionBuilder) -> lir::Function {
        let builder = FunctionBuilder { lir_builder: self, id, name, ret: None, parameters: Vec::new(), counter: Counter::new(0) };
        let func = f(builder);
        let sig = func.signature();
        if self.functions.get_mut(&id).unwrap().replace(func).is_some() {
            panic!("function already defined")
        };
        sig
    }

    pub fn declare_struct(&mut self) -> lir::StructID {
        let id = lir::StructID(self.structs.len());
        if self.structs.insert(id, None).is_some() {
            panic!("struct already declared")
        };
        id
    }

    pub fn define_struct<F>(&mut self, id: lir::StructID, name: String, f: F) where F: FnOnce(&mut StructBuilder) {
        let mut builder = StructBuilder { id, name, fields: Vec::new() };
        f(&mut builder);
        if self.structs.get_mut(&id).unwrap().replace(builder.finish()).is_some() {
            panic!("struct already defined");
        };
    }

    pub fn main_function(&mut self, id: lir::FunctionID) {
        if self.main_function.replace(id).is_some() {
            panic!("main function already defined");
        }
    }
}

pub struct StructBuilder {
    id: lir::StructID,
    name: String,
    fields: Vec<lir::StructField>
}

impl StructBuilder {
    pub fn field(&mut self, name: String, ty: lir::Type) {
        self.fields.push(lir::StructField { name, ty });
    }

    fn finish(self) -> lir::Struct {
        let StructBuilder { id, name, fields } = self;
        lir::Struct {
            id,
            name,
            fields
        }
    }
}

pub struct FunctionBuilder<'a> {
    lir_builder: &'a mut LIRBuilder,

    id: lir::FunctionID,
    name: String,
    ret: Option<lir::Type>,
    parameters: Vec<lir::FunctionParameter>,

    counter: Counter,
}

impl FunctionBuilder<'_> {
    pub fn parameter(&mut self, name: String, ty: lir::Type) -> lir::LocalID {
        let local = lir::LocalID(self.counter.next());
        self.parameters.push(lir::FunctionParameter { local, name, ty });
        local
    }

    pub fn return_type(&mut self, ty: lir::Type) {
        self.ret = Some(ty);
    }

    pub fn return_void(&mut self) {
        self.ret = None;
    }

    pub fn build<F>(self, f: F) -> lir::Function where F: FnOnce(BlockBuilder) -> lir::BlockDiverge {
        let FunctionBuilder { lir_builder, id, name, ret, parameters, mut counter } = self;

        let block = {
            let mut block_builder: BlockBuilder = BlockBuilder::new(lir_builder, &mut counter);
            for param in &parameters {
                block_builder.locals.insert(param.local, param.ty.clone());
            }
            f(block_builder)
        };

        lir::Function {
            id,
            name,
            ret,
            parameters,
            block
        }
    }
}


pub struct BlockBuilder<'a> {
    pub lir_builder: &'a mut LIRBuilder,
    counter: &'a mut Counter,

    instructions: Vec<lir::Instruction>,

    stack_types: Vec<lir::Type>,
    locals: HashMap<lir::LocalID, lir::Type>
}

impl<'a> BlockBuilder<'a> {
    fn new(builder: &'a mut LIRBuilder, counter: &'a mut Counter) -> BlockBuilder<'a> {
        BlockBuilder {
            lir_builder: builder,
            counter,
            instructions: Vec::new(),
            stack_types: Vec::new(),
            locals: HashMap::new()
        }
    }

    pub fn build<T>(&mut self, f: impl FnOnce(BlockBuilder) -> T) -> T {
        f(BlockBuilder::new(self.lir_builder, self.counter))
    }

    fn push_type(&mut self, ty: lir::Type) {
        self.stack_types.push(ty);
    }

    fn pop_type(&mut self, eq_ty: &lir::Type) {
        let ty = self.stack_types.pop().unwrap();
        if &ty != eq_ty {
            panic!("Expected {:?}, got {:?}", eq_ty, ty);
        }
    }

    pub fn upcast_ref(&mut self, from_type: lir::Type) {
        if !matches!(&from_type, lir::Type::StructRef(_)) {
            panic!();
        }
        self.pop_type(&from_type);
        self.push_type(lir::Type::AnyRef);
    }

    pub fn downcast_ref(&mut self, to_type: lir::Type) {
        if !matches!(&to_type, lir::Type::StructRef(_)) {
            panic!();
        }
        self.pop_type(&lir::Type::AnyRef);
        self.push_type(to_type);
    }

    pub fn cast_ref(&mut self, from_type: lir::StructID, to_type: lir::StructID) {
        self.pop_type(&lir::Type::StructRef(from_type));
        self.push_type(lir::Type::StructRef(to_type));
    }

    pub fn declare_variable(&mut self, ty: lir::Type) -> lir::LocalID {
        self.pop_type(&ty);
        let id = lir::LocalID(self.counter.next());
        self.locals.insert(id, ty);
        self.instructions.push(lir::Instruction::DeclareLocal(id));
        id
    }

    pub fn load_i32(&mut self, value: i32) {
        self.push_type(lir::Type::Int32);
        self.instructions.push(lir::Instruction::LoadI32(value));
    }

    pub fn load_bool(&mut self, value: bool) {
        self.push_type(lir::Type::Boolean);
        self.instructions.push(lir::Instruction::LoadBool(value));
    }

    pub fn load_variable(&mut self, id: lir::LocalID) -> lir::Type {
        self.push_type(self.locals[&id].clone());
        self.instructions.push(lir::Instruction::LoadLocal(id));
        self.locals[&id].clone()
    }

    pub fn load_function(&mut self, id: lir::FunctionID, ty: lir::Type) {
        assert!(matches!(ty, lir::Type::Function(_, _)));
        self.push_type(ty.clone());
        self.instructions.push(lir::Instruction::LoadFunction(id));
    }

    pub fn load_null(&mut self) {
        self.push_type(lir::Type::AnyRef);
        self.instructions.push(lir::Instruction::LoadNull);
    }

    pub fn create_tuple(&mut self, tys: Vec<lir::Type>) {
        for ty in tys.iter().rev() {
            self.pop_type(ty);
        }
        self.instructions.push(lir::Instruction::CreateTuple(tys.len()));
        self.push_type(lir::Type::Tuple(tys));
    }

    pub fn splat(&mut self, tys: Vec<lir::Type>) {
        self.pop_type(&lir::Type::Tuple(tys.clone()));
        self.instructions.push(lir::Instruction::Splat);
        for ty in tys {
            self.push_type(ty)
        }
    }

    pub fn create_zero_init_struct(&mut self, struct_: lir::StructID) {
        self.push_type(lir::Type::StructRef(struct_));
        self.instructions.push(lir::Instruction::CreateZeroInitStruct(struct_));
    }

    pub fn create_struct(&mut self, struct_: lir::StructID, fields: Vec<(String, lir::Type)>) {
        let mut field_names = Vec::new();
        for (name, ty) in fields.into_iter().rev() {
            self.pop_type(&ty);
            field_names.push(name);
        }
        self.push_type(lir::Type::StructRef(struct_));
        self.instructions.push(lir::Instruction::CreateStruct(struct_, field_names));
    }

    pub fn get_field(&mut self, struct_: lir::StructID, field: (String, lir::Type)) {
        self.pop_type(&lir::Type::StructRef(struct_));
        self.push_type(field.1);
        self.instructions.push(lir::Instruction::GetField(struct_, field.0));
    }

    pub fn set_field(&mut self, struct_: lir::StructID, field: (String, lir::Type)) {
        let (field_name, field_type) = field;
        self.pop_type(&field_type);
        self.pop_type(&lir::Type::StructRef(struct_));
        self.instructions.push(lir::Instruction::SetField(struct_, field_name));
        self.push_type(lir::Type::StructRef(struct_));
    }

    pub fn call(&mut self, params: &[lir::Type], ret: lir::Type) {
        for ty in params.iter().rev() {
            self.pop_type(ty);
        }
        self.pop_type(&lir::Type::Function(params.to_vec(), Some(Box::new(ret.clone()))));
        self.push_type(ret);
        self.instructions.push(lir::Instruction::Call(params.len()));
    }

    pub fn call_void(&mut self, params: &[lir::Type]) {
        for ty in params.iter().rev() {
            self.pop_type(ty);
        }
        self.pop_type(&lir::Type::Function(params.to_vec(), None));
        self.instructions.push(lir::Instruction::CallVoid(params.len()));
    }

    pub fn pop(&mut self, ty: lir::Type) {
        self.pop_type(&ty);
        self.instructions.push(lir::Instruction::Pop);
    }

    pub fn return_value(&mut self, ty: lir::Type) {
        self.pop_type(&ty);
        self.instructions.push(lir::Instruction::Return);
    }

    pub fn return_void(&mut self) {
        self.instructions.push(lir::Instruction::ReturnVoid);
    }

    pub fn statepoint(&mut self) {
        self.instructions.push(lir::Instruction::StatePoint);
    }

    pub fn unreachable(&mut self) {
        self.instructions.push(lir::Instruction::Unreachable);
    }

    pub fn block_value(&mut self, block: lir::BlockValue) {
        let yield_ty = block.yield_type.clone();
        self.instructions.push(lir::Instruction::BlockValue(block));
        self.push_type(yield_ty);
    }

    pub fn block_void(&mut self, block: lir::BlockVoid) {
        self.instructions.push(lir::Instruction::BlockVoid(block));
    }

    pub fn block_diverge(&mut self, block: lir::BlockDiverge) {
        self.instructions.push(lir::Instruction::BlockDiverge(block));
    }

    pub fn if_else_value(&mut self, ty: lir::Type, then_do: lir::BlockValueOrDiverge, else_do: lir::BlockValueOrDiverge) {
        self.instructions.push(lir::Instruction::IfElseValue { then_do, else_do, ty: ty.clone() });
        self.push_type(ty);
    }

    pub fn if_else_void(&mut self, then_do: lir::BlockVoidOrDiverge, else_do: lir::BlockVoidOrDiverge) {
        self.instructions.push(lir::Instruction::IfElseVoid { then_do, else_do });
    }

    pub fn if_else_diverge(&mut self, then_do: lir::BlockDiverge, else_do: lir::BlockDiverge) {
        self.instructions.push(lir::Instruction::IfElseDiverge { then_do, else_do });
    }

    pub fn yield_none(self) -> lir::BlockVoid {
        let BlockBuilder { instructions, .. } = self;

        lir::BlockVoid {
            instructions
        }
    }

    pub fn yield_diverges(self) -> lir::BlockDiverge {
        let BlockBuilder { instructions, .. } = self;

        lir::BlockDiverge {
            instructions
        }
    }

    pub fn yield_value(mut self, ty: lir::Type) -> lir::BlockValue {
        self.pop_type(&ty);

        let BlockBuilder { instructions, .. } = self;

        lir::BlockValue {
            instructions,
            yield_type: ty
        }
    }
}
#![allow(dead_code)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
use indexmap::IndexMap;

use crate::util::map_join;

#[derive(Clone)]
pub struct FunctionRef(Rc<Function>);

impl Value for FunctionRef {
    fn emit_value(&self) -> String {
        format!(r"@{}", self.0.name)
    }

    fn ty(&self) -> TypeRef {
        Types::ptr()
    }
}

impl FunctionRef {
    pub fn memory(self, access: MemoryAccess) -> Self {
        self.0.attrs.borrow_mut().memory = access;
        self
    }

    pub fn willreturn(self) -> Self {
        self.0.attrs.borrow_mut().willreturn = true;
        self
    }

    pub fn nounwind(self) -> Self {
        self.0.attrs.borrow_mut().nounwind = true;
        self
    }

    pub fn ret_noalias(self) -> Self {
        self.0.attrs.borrow_mut().ret_noalias = true;
        self
    }
    
    pub fn parameters(&self) -> &[ParameterRef] {
        &self.0.parameters
    }
}

#[derive(Clone, Debug)]
pub struct StructRef(Rc<NamedStruct>);

impl StructRef {
    pub fn set_fields(&self, fields: Vec<TypeRef>) {
        self.0.set_fields(fields)
    }

    pub fn as_type_ref(&self) -> TypeRef {
        Rc::new(Type::NamedStruct(self.clone()))
    }
}

pub type TypeRef = Rc<Type>;

#[derive(Debug)]
pub enum Type {
    Void,
    Pointer,
    Integer(u8),
    Array(TypeRef, usize),
    NamedStruct(StructRef),
    Struct(Vec<TypeRef>),
    Function(TypeRef, Vec<TypeRef>)
}

impl Type {
    pub fn emit(&self) -> String {
        match self {
            Type::Void => "void".into(),
            Type::Pointer => "ptr".into(),
            Type::Integer(bits) => format!("i{}", bits),
            Type::Array(t, count) => format!("[{} x {}]", count, t.emit()),
            Type::NamedStruct(s) => format!("%{}", s.0.name),
            Type::Struct(items) => {
                let items: Vec<String> = items.iter().map(|t| t.emit()).collect();
                format!("{{ {} }}", items.join(", "))
            }
            Type::Function(ret, params) => {
                let items: Vec<String> = params.iter().map(|t| t.emit()).collect();
                format!("{} ({})", ret.emit(), items.join(", "))
            }
        }
    }

    fn type_at_index(&self, index: Option<usize>) -> TypeRef {
        match self {
            Type::NamedStruct(struct_ref) => {
                Rc::clone(&struct_ref.0.fields.borrow().clone().unwrap()[index.unwrap()])
            }
            Type::Struct(fields) => {
                Rc::clone(&fields[index.unwrap()])
            }
            Type::Array(elem, ..) => {
                Rc::clone(elem)
            },
            _ => {
                panic!("{:?}", self);
            }
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Type::Void => 0,
            Type::Pointer => 8,  // todo
            Type::Integer(bits) => (*bits / 8) as usize,
            Type::Array(base, count) => base.size() * *count,
            Type::NamedStruct(struct_) => struct_.0.size(),
            Type::Struct(fields) => size_of_struct(fields),
            Type::Function(_, _) => 8  // todo
        }
    }

    pub fn align(&self) -> usize {
        match self {
            Type::Void => 1,
            Type::Pointer => 8,  // todo
            Type::Integer(bits) => ((*bits / 8) as usize).max(1),
            Type::Array(base, _) => base.align(),
            Type::NamedStruct(struct_) => struct_.0.align(),
            Type::Struct(fields) => align_of_struct(fields),
            Type::Function(_, _) => 8 // todo
        }
    }
}

pub struct Types {
    named_structs: IndexMap<String, StructRef>
}

impl Types {
    pub fn new() -> Types {
        Types {
            named_structs: IndexMap::new()
        }
    }

    pub fn add_struct(&mut self, name: impl Into<String>) -> StructRef {
        let name = name.into();
        let item = StructRef(Rc::new(NamedStruct { name: format!(r#""{}""#, &name), fields: RefCell::new(None) }));
        self.named_structs.insert(name, item.clone());
        item
    }

    pub fn sizeof(ty: &TypeRef) -> Constant {
        Constant::Sizeof(Rc::clone(ty))
    }

    pub fn struct_(fields: Vec<TypeRef>) -> TypeRef {
        Rc::new(Type::Struct(fields))
    }

    pub fn struct_constant(items: Vec<Constant>) -> Constant {
        let ty = Types::struct_(items.iter().map(|item| item.ty()).collect());
        Constant::Struct { items, ty }
    }

    // pub fn struct_literal(items: Vec<ValueRef>) -> ValueRef {
    //     StructLiteral { elements: items }.to_value()
    // }

    pub fn array(elem: &TypeRef, count: usize) -> TypeRef {
        Rc::new(Type::Array(Rc::clone(elem), count))
    }

    pub fn function(ret: TypeRef, params: Vec<TypeRef>) -> TypeRef {
        Rc::new(Type::Function(ret, params))
    }

    pub fn function_constant(func: &FunctionRef) -> Constant {
        Constant::Function(func.clone())
    }

    pub fn void() -> TypeRef {
        Rc::new(Type::Void)
    }

    pub fn ptr() -> TypeRef {
        Rc::new(Type::Pointer)
    }

    pub fn int(bits: u8) -> TypeRef {
        Rc::new(Type::Integer(bits))
    }

    pub fn int_constant(bits: u8, value: u64) -> Constant {
        Constant::Integer { ty: Types::int(bits), value }
    }

    pub fn zeroinit(ty: TypeRef) -> Constant {
        Constant::ZeroInitializer(ty)
    }

    pub fn null() -> Constant {
        Constant::Null
    }

    fn emit(&self) -> String {
        let mut types = String::new();
        for s in self.named_structs.values() {
            types += s.0.emit().as_str();
            types += "\n";
        }
        types
    }
}

pub struct Module {
    pub name: String,
    functions: IndexMap<String, FunctionRef>,
    globals: IndexMap<String, GlobalRef>,
    pub types: Types
}

impl Module {
    pub fn new(name: impl Into<String>) -> Module {
        Module {
            name: name.into(),
            functions: IndexMap::new(),
            globals: IndexMap::new(),
            types: Types::new(),
        }
    }

    pub fn add_function(&mut self, name: impl Into<String>, ret: TypeRef, parameters: Vec<ParameterRef>, cc: CallingConvention) -> FunctionRef {
        let name = name.into();
        let param_types: Vec<TypeRef> = parameters.iter().map(|p| Rc::clone(&p.0.typ)).collect();
        let item = FunctionRef(Rc::new(Function {
            name: format!(r#""{}""#, &name),
            cc,
            ret: Rc::clone(&ret), parameters,
            blocks: RefCell::new(vec![]),
            ty: Types::function(ret, param_types),
            attrs: Default::default()
        }));
        self.functions.insert(name, item.clone());
        item
    }

    pub fn add_global_constant(&mut self, name: String, initializer: Constant) -> GlobalRef {
        let global = GlobalRef(Rc::new(Global { name: format!("\"{}\"", &name), typ: initializer.ty(), is_const: true, initializer: RefCell::new(initializer) }));
        self.globals.insert(name, global.clone());
        global
    }

    pub fn emit(&self) -> String {
        let mut module = String::new();
        module += self.types.emit().as_str();

        for global in self.globals.values() {
            module += global.0.emit().as_str();
        }

        for func in self.functions.values() {
            module += func.0.emit().as_str();
        }

        module
    }
}

pub trait Value {
    fn emit_value(&self) -> String;

    fn ty(&self) -> TypeRef;

    fn to_value(self) -> ValueRef where Self: Sized + 'static {
        Rc::new(self)
    }
}

#[derive(Clone)]
pub enum Constant {
    Null,
    ZeroInitializer(TypeRef),
    Integer {
        ty: TypeRef,
        value: u64
    },
    String {
        ty: TypeRef,
        chars: Vec<u8>
    },
    Array {
        ty: TypeRef,
        elem_ty: TypeRef,
        items: Vec<Constant>,
    },
    Struct {
        ty: TypeRef,
        items: Vec<Constant>,
    },
    Boolean(TypeRef, bool),
    Function(FunctionRef),
    Sizeof(TypeRef)
}

impl Value for Constant {
    fn emit_value(&self) -> String {
        match self {
            Constant::Null => "null".into(),
            Constant::ZeroInitializer(_) => "zeroinitializer".into(),
            Constant::Integer { value, .. } => {
                format!("{}", value)
            },
            Constant::String { chars, .. } => {
                format!("c\"{}\"", String::from_utf8(chars.clone()).unwrap())
            },
            Constant::Array { elem_ty, items, .. } => {
                let items: Vec<String> = items.iter().map(|item| elem_ty.emit() + " " + &item.emit_value()).collect();
                format!("[ {} ]", items.join(", "))
            }
            Constant::Struct { items, .. } => {
                let items: Vec<String> = items.iter().map(|item| item.ty().emit() + " " + &item.emit_value()).collect();
                format!("{{ {} }}", items.join(", "))
            }
            Constant::Boolean(_, value) => {
                if *value { "true".into() } else { "false".into() }
            }
            Constant::Function(func) => {
                format!("@{}", &func.0.name)
            }
            Constant::Sizeof(ty) => {
                format!("ptrtoint (ptr getelementptr ({}, ptr null, i32 1) to i64)", ty.emit())
            }
        }
    }

    fn ty(&self) -> TypeRef {
        match self {
            Constant::Null => Types::ptr(),
            Constant::ZeroInitializer(ty) => Rc::clone(ty),
            Constant::Integer  { ty, .. } => Rc::clone(ty),
            Constant::String   { ty, .. } => Rc::clone(ty),
            Constant::Array    { ty, .. } => Rc::clone(ty),
            Constant::Struct   { ty, .. } => Rc::clone(ty),
            Constant::Boolean  ( ty, _  ) => Rc::clone(ty),
            Constant::Function(_) => Types::ptr(),
            Constant::Sizeof(..) => Types::int(64)
        }
    }
}

// struct StructLiteral {
//     elements: Vec<ValueRef>
// }
//
// impl Value for StructLiteral {
//     fn emit_value(&self) -> String {
//         format!("{{ {} }}", map_join(&self.elements, |item| item.ty().emit() + " " + &item.emit_value()))
//     }
//
//     fn ty(&self) -> TypeRef {
//         Types::struct_(self.elements.iter().map(|item| item.ty()).collect())
//     }
// }

#[derive(Clone)]
pub struct GlobalRef(Rc<Global>);

pub struct Global {
    name: String,
    typ: TypeRef,
    is_const: bool,
    initializer: RefCell<Constant>
}

impl Global {
    fn emit(&self) -> String {
        format!("@{} = {} {} {}\n", &self.name, if self.is_const { "constant" } else { "global" }, self.typ.emit(), self.initializer.borrow().emit_value())
    }
}

impl Value for GlobalRef {
    fn emit_value(&self) -> String {
        format!("@{}", &self.0.name)
    }

    fn ty(&self) -> TypeRef {
        Types::ptr()
    }
}

impl GlobalRef {
    pub fn initialize(&mut self, constant: Constant) {
        *self.0.initializer.borrow_mut() = constant;
    }
}

#[derive(Default)]
pub enum CallingConvention {
    #[default]
    CCC,
    FastCC
}

impl CallingConvention {
    fn emit(&self) -> String {
        match self {
            CallingConvention::CCC => "ccc".into(),
            CallingConvention::FastCC => "fastcc".into()
        }
    }
}

struct Function {
    name: String,
    cc: CallingConvention,
    ret: TypeRef,
    pub parameters: Vec<ParameterRef>,
    blocks: RefCell<Vec<BasicBlockRef>>,
    ty: TypeRef,

    attrs: RefCell<FunctionAttributes>
}

#[derive(Default, Copy, Clone, Eq, PartialEq)]
pub enum MemoryAccess {
    None,
    Read,
    Write,
    #[default]
    ReadWrite
}

#[derive(Default)]
struct FunctionAttributes {
    memory: MemoryAccess,
    willreturn: bool,
    nounwind: bool,

    ret_noalias: bool,
}

impl MemoryAccess {
    fn emit(&self) -> String {
        match self {
            MemoryAccess::None => "memory(none) ".into(),
            MemoryAccess::Read => "memory(read) ".into(),
            MemoryAccess::Write => "memory(write) ".into(),
            MemoryAccess::ReadWrite => "".into()
        }
    }
}

impl FunctionAttributes {
    fn emit(&self) -> String {
        let mut attrs = String::from(" ");
        attrs += &self.memory.emit();
        if self.willreturn {
            attrs += "willreturn ";
        }
        if self.nounwind {
            attrs += "nounwind ";
        }
        attrs
    }

    fn emit_ret(&self) -> String {
        let mut attrs = String::from(" ");
        if self.ret_noalias {
            attrs += "noalias ";
        }
        attrs
    }
}

impl Function {
    fn add_block(&self, name: impl Into<String>) -> BasicBlockRef {
        let item = Rc::new(BasicBlock {
            label: name.into(),
            instructions: RefCell::new(vec![]),
            terminator: RefCell::new(None)
        });
        self.blocks.borrow_mut().push(Rc::clone(&item));
        item
    }

    fn emit(&self) -> String {
        let arguments: String = map_join(&self.parameters, |t| t.0.emit());

        let cc = self.cc.emit();

        if !self.blocks.borrow().is_empty() {
            let mut blocks = String::new();
            for block in self.blocks.borrow().iter() {
                blocks += block.emit().as_str();
            }
            format!("define {cc}{}{} @{}({}){}{{\n{}}}\n\n", self.attrs.borrow().emit_ret(), self.ret.emit(), self.name, arguments, self.attrs.borrow().emit(), blocks)
        } else {
            format!("declare {cc}{}{} @{}({}){}\n\n", self.attrs.borrow().emit_ret(), self.ret.emit(), self.name, arguments, self.attrs.borrow().emit())
        }
    }
}

#[derive(Clone)]
pub struct ParameterRef(Rc<Parameter>);

impl ParameterRef {
    pub fn new(name: impl AsRef<str>, typ: TypeRef) -> ParameterRef {
        ParameterRef(Rc::new(Parameter { name: format!(r#""{}""#, name.as_ref()), typ, attrs: Default::default() }))
    }

    pub fn noalias(self) -> ParameterRef {
        self.0.attrs.borrow_mut().noalias = true;
        self
    }

    pub fn nocapture(self) -> ParameterRef {
        self.0.attrs.borrow_mut().nocapture = true;
        self
    }

    pub fn nofree(self) -> ParameterRef {
        self.0.attrs.borrow_mut().nofree = true;
        self
    }

    pub fn noread(self) -> ParameterRef {
        self.0.attrs.borrow_mut().noread = true;
        self
    }

    pub fn nowrite(self) -> ParameterRef {
        self.0.attrs.borrow_mut().nowrite = true;
        self
    }
}

struct Parameter {
    name: String,
    typ: TypeRef,
    attrs: RefCell<ParameterAttributes>
}

#[derive(Default)]
struct ParameterAttributes {
    noalias: bool,
    nocapture: bool,
    nofree: bool,
    noread: bool,
    nowrite: bool,
}

impl ParameterAttributes {
    fn emit(&self) -> String {
        let mut attrs = String::from(" ");
        if self.noalias {
            attrs.push_str("noalias ")
        }
        if self.nocapture {
            attrs.push_str("nocapture ")
        }
        if self.nofree {
            attrs.push_str("nofree ")
        }
        if self.noread && self.nowrite {
            attrs.push_str("readnone ")
        } else if self.noread {
            attrs.push_str("writeonly ")
        } else if self.nowrite {
            attrs.push_str("readonly ")
        }
        attrs
    }
}

impl Parameter {
    fn emit(&self) -> String {
        format!("{}{}%{}", self.typ.emit(), self.attrs.borrow().emit(), self.name)
    }
}

impl Value for ParameterRef {
    fn emit_value(&self) -> String {
        format!("%{}", &self.0.name)
    }

    fn ty(&self) -> TypeRef {
        Rc::clone(&self.0.typ)
    }
}

pub struct BlockToken(FunctionRef, BasicBlockRef);

pub struct Builder {
    func: FunctionRef,
    mangle_register: RefCell<u32>,
    mangle_block: RefCell<u64>,
    curr: BasicBlockRef
}

impl Builder {
    pub fn new(func: &FunctionRef) -> Builder {
        let entry = func.0.add_block("entry");
        Builder {
            func: func.clone(), mangle_register: RefCell::new(0), mangle_block: RefCell::new(0), curr: entry
        }
    }

    pub fn build<F: FnOnce(&mut Builder)>(&mut self, tok: BlockToken, f: F) {
        let mut builder = Builder { mangle_register: self.mangle_register.clone(), mangle_block: self.mangle_block.clone(), func: tok.0, curr: tok.1 };
        f(&mut builder);
        *self.mangle_register.borrow_mut() = builder.mangle_register.into_inner();
        *self.mangle_block.borrow_mut() = builder.mangle_block.into_inner();
    }

    pub fn new_block(&self, name: Option<String>) -> BlockToken {
        let name = name.map(Into::into).unwrap_or_else(|| format!("block_{}", self.next_block()));
        let block_ref = self.func.0.add_block(name);
        BlockToken(self.func.clone(), block_ref)
    }

    pub fn goto_block(&mut self, block: BlockToken) {
        self.curr = block.1;
    }

    fn next_register(&self) -> String {
        let num = *self.mangle_register.borrow();
        *self.mangle_register.borrow_mut() += 1;
        format!("{}", num)
    }

    fn next_block(&self) -> String {
        let num = *self.mangle_block.borrow();
        *self.mangle_block.borrow_mut() += 1;
        format!("{}", num)
    }

    pub fn ret(&self, val: ValueRef) {
        assert!(self.curr.terminator.borrow().is_none());
        *self.curr.terminator.borrow_mut() = Some(Terminator::Return(val))
    }

    pub fn ret_void(&self) {
        assert!(self.curr.terminator.borrow().is_none());
        *self.curr.terminator.borrow_mut() = Some(Terminator::ReturnVoid)
    }

    pub fn branch(&self, target: &BlockToken) {
        assert!(Rc::ptr_eq(&self.func.0, &target.0.0));
        assert!(self.curr.terminator.borrow().is_none());
        *self.curr.terminator.borrow_mut() = Some(Terminator::Branch(target.1.clone()));
    }

    pub fn cbranch(&self, cond: ValueRef, if_true: &BlockToken, if_false: &BlockToken) {
        assert!(Rc::ptr_eq(&self.func.0, &if_true.0.0));
        assert!(Rc::ptr_eq(&self.func.0, &if_false.0.0));

        assert!(self.curr.terminator.borrow().is_none());
        *self.curr.terminator.borrow_mut() = Some(Terminator::CondBranch(cond, if_true.1.clone(), if_false.1.clone()));
    }

    pub fn alloca(&self, name: Option<String>, ty: TypeRef) -> InstrRef {
        let name = name.map(Into::into).unwrap_or_else(|| self.next_register());
        let instr_ref = InstrRef::new(Instruction::Alloca { name, ty });
        self.curr.instructions.borrow_mut().push(instr_ref.clone());
        instr_ref
    }

    pub fn call(&self, name: Option<String>, cc: CallingConvention, ret: TypeRef, func: &ValueRef, args: Vec<ValueRef>) -> InstrRef {
        let name = name.unwrap_or_else(|| self.next_register());
        let instr_ref = InstrRef::new(Instruction::Call { name, cc, ret, func: Rc::clone(func), args});
        self.curr.instructions.borrow_mut().push(instr_ref.clone());
        instr_ref
    }

    pub fn call_void(&self, cc: CallingConvention, func: &ValueRef, args: Vec<ValueRef>) {
        let instr_ref = InstrRef::new(Instruction::CallVoid { cc, func: Rc::clone(func), args});
        self.curr.instructions.borrow_mut().push(instr_ref.clone());
    }

    pub fn gep(&self, name: Option<String>, ty: TypeRef, base: ValueRef, indices: Vec<GEPIndex>) -> InstrRef {
        let name = name.unwrap_or_else(|| self.next_register());
        let instr_ref = InstrRef::new(Instruction::GetElementPointer { name, ty, base, indices });
        self.curr.instructions.borrow_mut().push(instr_ref.clone());
        instr_ref
    }

    // pub fn gep(&self, name: Option<String>, base: ValueRef, indices: Vec<GEPIndex>) -> InstrRef {
    //     let name = name.unwrap_or_else(|| self.next());
    //     let instr_ref = InstrRef::new(Instruction::GetElementPointer { name, ty: base.ty(), base, indices });
    //     self.curr.instructions.borrow_mut().push(instr_ref.clone());
    //     instr_ref
    // }

    pub fn load(&self, name: Option<String>, ty: TypeRef, ptr: ValueRef) -> InstrRef {
        let name = name.unwrap_or_else(|| self.next_register());
        let instr_ref = InstrRef::new(Instruction::Load { name, ty, ptr });
        self.curr.instructions.borrow_mut().push(instr_ref.clone());
        instr_ref
    }

    pub fn store(&self, ptr: impl Borrow<ValueRef>, value: impl Borrow<ValueRef>) -> InstrRef {
        let instr_ref = InstrRef::new(Instruction::Store { ptr: Rc::clone(ptr.borrow()), value: Rc::clone(value.borrow()) });
        self.curr.instructions.borrow_mut().push(instr_ref.clone());
        instr_ref
    }

    pub fn extractvalue(&self, name: Option<String>, base: &ValueRef, indices: Vec<u32>) -> InstrRef {
        assert!(matches!(base.ty().as_ref(), Type::NamedStruct(_) | Type::Struct(_)), "Actual Type: {:?}", base.ty());
        let name = name.unwrap_or_else(|| self.next_register());
        let instr_ref = InstrRef::new(Instruction::ExtractValue { name, base: Rc::clone(base), indices });
        self.curr.instructions.borrow_mut().push(instr_ref.clone());
        instr_ref
    }

    pub fn insertvalue(&self, name: Option<String>, base: &ValueRef, value: ValueRef, indices: Vec<u32>) -> InstrRef {
        assert!(matches!(base.ty().as_ref(), Type::NamedStruct(_) | Type::Struct(_)), "Actual Type: {:?}", base.ty());
        let name = name.unwrap_or_else(|| self.next_register());
        let instr_ref = InstrRef::new(Instruction::InsertValue { name, base: Rc::clone(base), value, indices });
        self.curr.instructions.borrow_mut().push(instr_ref.clone());
        instr_ref
    }
}

pub type BasicBlockRef = Rc<BasicBlock>;

pub struct BasicBlock {
    label: String,
    instructions: RefCell<Vec<InstrRef>>,
    terminator: RefCell<Option<Terminator>>
}

impl BasicBlock {
    fn emit(&self) -> String {
        let mut s = format!("{}:\n", self.label);
        for instr in self.instructions.borrow().iter() {
            s += "  ";
            s += instr.0.emit().as_str();
        }
        s += "  ";
        s += &self.terminator.borrow().as_ref().map(|t| t.emit()).unwrap_or("<no terminator>\n".into());
        s
    }

    fn emit_ref(&self) -> String {
        format!("label %{}", self.label)
    }
}

#[derive(Clone)]
pub struct InstrRef(Rc<Instruction>);

impl InstrRef {
    fn new(instr: Instruction) -> InstrRef {
        InstrRef(Rc::new(instr))
    }
}

pub type ValueRef = Rc<dyn Value>;

pub enum GEPIndex {
    ConstantIndex(u32),
    ArrayIndex(ValueRef),
}

pub enum Instruction {
    Add { name: String, ret: TypeRef, left: ValueRef, right: ValueRef },
    Alloca { name: String, ty: TypeRef },
    Call { name: String, cc: CallingConvention, ret: TypeRef, func: ValueRef, args: Vec<ValueRef> },
    CallVoid { cc: CallingConvention, func: ValueRef, args: Vec<ValueRef> },
    GetElementPointer { name: String, ty: TypeRef, base: ValueRef, indices: Vec<GEPIndex> },
    Load { name: String, ty: TypeRef, ptr: ValueRef },
    Store { ptr: ValueRef, value: ValueRef },
    ExtractValue { name: String, base: ValueRef, indices: Vec<u32> },
    InsertValue { name: String, base: ValueRef, value: ValueRef, indices: Vec<u32> },
}

impl Instruction {
    fn emit(&self) -> String {
        match self {
            Instruction::Add { name, ret, left, right} => {
                format!("%{} = add {} {}, {}\n", name, ret.emit(), left.emit_value(), right.emit_value())
            },
            Instruction::Alloca { name, ty } => {
                format!("%{name} = alloca {}\n", ty.emit())
            }
            Instruction::Call { name, cc, ret, func, args } => {
                let args: Vec<String> = args.iter().map(|a| a.ty().emit() + " " + &a.emit_value()).collect();
                format!("%{name} = call {} {} {}({})\n", cc.emit(), ret.emit(), func.emit_value(), args.join(", "))
            }
            Instruction::CallVoid { cc, args, func } => {
                let args: Vec<String> = args.iter().map(|a| a.ty().emit() + " " + &a.emit_value()).collect();
                format!("call {} void {}({})\n", cc.emit(), func.emit_value(), args.join(", "))
            }
            Instruction::GetElementPointer { name, ty, base, indices } => {
                let mut rendered_indices = vec![];
                for index in indices {
                    match index {
                        GEPIndex::ConstantIndex(value) => {
                            rendered_indices.push(format!("i32 {value}"))
                        }
                        GEPIndex::ArrayIndex(value) => {
                            rendered_indices.push(format!("{} {}", value.ty().emit(), value.emit_value()))
                        }
                    }
                }
                format!("%{name} = getelementptr {}, ptr {}, {}\n", ty.emit(), base.emit_value(), rendered_indices.join(", "))
            },
            Instruction::Load { name, ty, ptr } => {
                format!("%{name} = load {}, ptr {}\n", ty.emit(), ptr.emit_value())
            }
            Instruction::Store { ptr, value, .. } => {
                format!("store {} {}, ptr {}\n", value.ty().emit(), value.emit_value(), ptr.emit_value())
            }
            Instruction::ExtractValue { name, base, indices } => {
                let rendered: Vec<String> = indices.iter().map(|index| format!("{index}")).collect();
                format!("%{} = extractvalue {} {}, {}\n", name, base.ty().emit(), base.emit_value(), rendered.join(", "))
            }
            Instruction::InsertValue { name, base, value, indices } => {
                let rendered: Vec<String> = indices.iter().map(|index| format!("{index}")).collect();
                format!("%{} = insertvalue {} {}, {} {}, {}\n", name, base.ty().emit(), base.emit_value(), value.ty().emit(), value.emit_value(), rendered.join(", "))
            }
        }
    }
}

impl Value for InstrRef {
    fn emit_value(&self) -> String {
        match self.0.as_ref() {
            Instruction::Add { name, .. } => format!("%{}", name),
            Instruction::Alloca { name, .. } => format!("%{name}"),
            Instruction::Call { name, .. } => format!("%{name}"),
            Instruction::CallVoid { .. } => panic!(),
            Instruction::GetElementPointer { name, .. } => format!("%{name}"),
            Instruction::Load { name, .. } => format!("%{name}"),
            Instruction::Store { .. } => panic!(),
            Instruction::ExtractValue { name, .. } => format!("%{name}"),
            Instruction::InsertValue { name, .. } => format!("%{name}"),
        }
    }

    fn ty(&self) -> TypeRef {
        match self.0.as_ref() {
            Instruction::Add { ret, .. } => Rc::clone(ret),
            Instruction::Alloca { .. } => Types::ptr(),
            Instruction::Call { ret, .. } => Rc::clone(ret),
            Instruction::CallVoid { .. } => panic!(),
            Instruction::GetElementPointer { .. } => Types::ptr(),
            Instruction::Load { ty, .. } => Rc::clone(ty),
            Instruction::Store { .. } => panic!(),
            Instruction::ExtractValue { base, indices, .. } => {
                let mut curr = base.ty();
                for index in indices {
                    curr = curr.type_at_index(Some(*index as usize));
                }
                return curr;
            }
            Instruction::InsertValue { base, .. } => {
                base.ty()
            }
        }
    }
}

pub enum Terminator {
    Return(ValueRef),
    ReturnVoid,
    Branch(BasicBlockRef),
    CondBranch(ValueRef, BasicBlockRef, BasicBlockRef),
}

impl Terminator {
    fn emit(&self) -> String {
        match self {
            Terminator::Return(value) => {
                format!("ret {} {}\n", value.ty().emit(), value.emit_value())
            }
            Terminator::Branch(block) => {
                format!("br {}\n", block.emit_ref())
            }
            Terminator::CondBranch(cond, if_true, if_false) => {
                format!("br i1 {}, {}, {}\n", cond.emit_value(), if_true.emit_ref(), if_false.emit_ref())
            }
            Terminator::ReturnVoid => {
                format!("ret void\n")
            }
        }
    }
}

struct NamedStruct {
    name: String,
    fields: RefCell<Option<Vec<TypeRef>>>,
}

impl Debug for NamedStruct {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.name)
    }
}

impl NamedStruct {
    pub fn set_fields(&self, fields: Vec<TypeRef>) {
        *self.fields.borrow_mut() = Some(fields);
    }

    fn emit(&self) -> String {
        match self.fields.borrow().as_ref() {
            Some(fields) => {
                let formatted_items: Vec<String> = fields.iter().map(|t| t.emit()).collect();
                format!("%{} = type {{ {} }}\n", self.name, formatted_items.join(", "))
            },
            None => {
                format!("%{} = type opaque\n", self.name)
            }
        }
    }

    fn size(&self) -> usize {
        let fields = self.fields.borrow();
        if let Some(fields) = fields.as_ref() {
            size_of_struct(fields)
        } else {
            panic!()
        }
    }

    fn align(&self) -> usize {
        let fields = self.fields.borrow();
        if let Some(fields) = fields.as_ref() {
            align_of_struct(fields)
        } else {
            panic!()
        }
    }
}

fn size_of_struct(fields: &[TypeRef]) -> usize {
    let mut offset = 0;
    let mut largest_align = 1;
    for field in fields {
        let needed_align = field.align();
        if offset % needed_align != 0 {
            offset += needed_align - (offset % needed_align);
        }
        if needed_align > largest_align {
            largest_align = needed_align;
        }
        offset += field.size();
    }
    if offset % largest_align != 0 {
        offset += largest_align - (offset % largest_align);
    }
    offset
}

fn align_of_struct(fields: &[TypeRef]) -> usize {
    let mut largest_align = 1;
    for field in fields {
        let needed_align = field.align();
        if needed_align > largest_align {
            largest_align = needed_align;
        }
    }
    largest_align
}

#[cfg(test)]
mod test {
    use super::{Module, Types};

    #[test]
    fn test_create_module() {
        let _ = Module::new(String::from("test"));
    }

    #[test]
    fn test_types() {
        let mut m = Module::new(String::from("test"));
        m.types.add_struct("<test>").set_fields(vec![Types::int(32)]);

        m.emit();
    }

    // #[test]
    // fn test_function() {
    //     let mut m = Module::new(String::from("test"));
    //
    //     let f = m.add_function("main", Types::int(32), vec![]);
    //     let b = f.add_block("entry".into());
    //     b.ret(Types::int_constant(32, 0).to_value());
    //
    //     print!("{}", m.emit());
    // }
}
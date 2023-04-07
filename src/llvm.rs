use std::cell::RefCell;
use std::rc::{Rc, Weak};
use indexmap::IndexMap;
use crate::llvm;

pub type FunctionRef = Rc<Function>;

pub type StructRef = Rc<NamedStruct>;

pub type TypeRef = Rc<Type>;

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
            Type::Array(t, count) => format!("[{} x {}]", t.emit(), count),
            Type::NamedStruct(s) => format!("%{}", s.name),
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
                Rc::clone(&struct_ref.fields.borrow().clone().unwrap()[index.unwrap()])
            }
            Type::Struct(fields) => {
                Rc::clone(&fields[index.unwrap()])
            }
            Type::Array(elem, ..) => {
                Rc::clone(elem)
            },
            _ => panic!()
        }
    }
}

pub struct Types {
    named_structs: IndexMap<String, Rc<NamedStruct>>
}

impl Types {
    pub fn new() -> Types {
        Types {
            named_structs: IndexMap::new()
        }
    }

    pub fn add_struct(&mut self, name: String) -> StructRef {
        let item = Rc::new(NamedStruct { name: name.clone(), fields: RefCell::new(None) });
        self.named_structs.insert(name, Rc::clone(&item));
        item
    }

    pub fn struct_(fields: Vec<TypeRef>) -> TypeRef {
        Rc::new(Type::Struct(fields))
    }

    pub fn function(ret: TypeRef, params: Vec<TypeRef>) -> TypeRef {
        Rc::new(Type::Function(ret, params))
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

    fn emit(&self) -> String {
        let mut types = String::new();
        for s in self.named_structs.values() {
            types += s.emit().as_str();
            types += "\n";
        }
        types
    }
}

pub struct Module {
    pub name: String,
    functions: IndexMap<String, Rc<Function>>,
    globals: IndexMap<String, Global>,
    pub types: Types
}

impl Module {
    pub fn new(name: String) -> Module {
        Module {
            name,
            functions: IndexMap::new(),
            globals: IndexMap::new(),
            types: Types::new(),
        }
    }

    pub fn add_function(&mut self, name: &str, ret: TypeRef, parameters: Vec<Parameter>) -> FunctionRef {
        let param_types: Vec<TypeRef> = parameters.iter().map(|p| Rc::clone(&p.typ)).collect();
        let item = Rc::new(Function {
            name: name.into(),
            ret: Rc::clone(&ret), parameters,
            blocks: RefCell::new(vec![]),
            ty: Types::function(ret, param_types),
            mangle: RefCell::new(0)
        });
        self.functions.insert(name.into(), Rc::clone(&item));
        item
    }

    pub fn emit(&self) -> String {
        let mut module = String::new();
        module += self.types.emit().as_str();

        for func in self.functions.values() {
            module += func.emit().as_str();
        }

        module
    }
}

pub trait Value {
    fn emit_value(&self) -> String;

    fn ty(&self) -> TypeRef;
}

#[derive(Clone)]
pub enum Constant {
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
    Boolean(TypeRef, bool)
}

impl Constant {
    pub fn to_value(self) -> ValueRef {
        Rc::new(self)
    }
}

impl Value for Constant {
    fn emit_value(&self) -> String {
        match self {
            Constant::ZeroInitializer(_) => "zeroinitializer".into(),
            Constant::Integer { value, .. } => {
                format!("{}", value)
            },
            Constant::String { chars, .. } => {
                format!("c\"{}\"", String::from_utf8(chars.clone()).unwrap())
            },
            Constant::Array { elem_ty, items, .. } => {
                let items: Vec<String> = items.iter().map(|item| elem_ty.emit() + &item.emit_value()).collect();
                format!("[ {} ]", items.join(", "))
            }
            Constant::Struct { items, .. } => {
                let items: Vec<String> = items.iter().map(|item| item.ty().emit() + &item.emit_value()).collect();
                format!("{{ {} }}", items.join(", "))
            }
            Constant::Boolean(_, value) => {
                if *value { "true".into() } else { "false".into() }
            }
        }
    }

    fn ty(&self) -> TypeRef {
        Rc::clone(match self {
            Constant::ZeroInitializer(ty) => ty,
            Constant::Integer  { ty, .. } => ty,
            Constant::String   { ty, .. } => ty,
            Constant::Array    { ty, .. } => ty,
            Constant::Struct   { ty, .. } => ty,
            Constant::Boolean  ( ty, _  ) => ty
        })
    }
}

struct Global {
    typ: TypeRef,
    is_const: bool,
    initializer: Option<Constant>
}

pub struct Function {
    name: String,
    ret: TypeRef,
    parameters: Vec<Parameter>,
    blocks: RefCell<Vec<BasicBlockRef>>,
    ty: TypeRef,
    mangle: RefCell<u32>,
}

impl Function {
    pub fn add_block(self: &Rc<Function>, name: String) -> BasicBlockRef {
        let item = Rc::new(BasicBlock {
            func: Rc::downgrade(self),
            label: name,
            instructions: RefCell::new(vec![]),
            terminator: RefCell::new(None)
        });
        self.blocks.borrow_mut().push(Rc::clone(&item));
        item
    }

    fn emit(&self) -> String {
        let arguments: Vec<String> = self.parameters.iter().map(|t| t.emit()).collect();

        if !self.blocks.borrow().is_empty() {
            let mut blocks = String::new();
            for block in self.blocks.borrow().iter() {
                blocks += block.emit().as_str();
            }
            format!("define {} @{}({}) {{\n{}}}\n\n", self.ret.emit(), self.name, arguments.join(", "), blocks)
        } else {
            format!("declare {} @{}({})\n\n", self.ret.emit(), self.name, arguments.join(", "))
        }
    }

    fn next(&self) -> String {
        let num = *self.mangle.borrow();
        *self.mangle.borrow_mut() += 1;
        format!("{:>08X}", num)
    }

    pub fn as_value(self: Rc<Function>) -> ValueRef {
        Rc::new(self)
    }
}

impl Value for Rc<Function> {
    fn emit_value(&self) -> String {
        format!("@{}", self.name)
    }

    fn ty(&self) -> TypeRef {
        Rc::clone(&self.ty)
    }
}

pub struct Parameter {
    pub name: String,
    pub typ: TypeRef
}

impl Parameter {
    fn emit(&self) -> String {
        format!("{} %{}", self.typ.emit(), self.name)
    }
}

pub type BasicBlockRef = Rc<BasicBlock>;

pub struct BasicBlock {
    func: Weak<Function>,
    label: String,
    instructions: RefCell<Vec<InstrRef>>,
    terminator: RefCell<Option<Terminator>>
}

impl BasicBlock {
    pub fn ret(&self, val: ValueRef) {
        *self.terminator.borrow_mut() = Some(Terminator::Return(val))
    }

    pub fn alloca(&self, name: Option<String>, ty: TypeRef) -> InstrRef {
        let name = name.unwrap_or_else(|| self.func.upgrade().unwrap().next());
        let instr_ref = Rc::new(Instruction::Alloca { name, ty });
        self.instructions.borrow_mut().push(Rc::clone(&instr_ref));
        instr_ref
    }

    pub fn call(&self, name: Option<String>, ret: TypeRef, func: ValueRef, args: Vec<ValueRef>) -> InstrRef {
        let name = name.unwrap_or_else(|| self.func.upgrade().unwrap().next());
        let instr_ref = Rc::new(Instruction::Call { name, ret, func, args});
        self.instructions.borrow_mut().push(Rc::clone(&instr_ref));
        instr_ref
    }

    pub fn gep(&self, name: Option<String>, ty: TypeRef, base: ValueRef, indices: Vec<GEPIndex>) -> InstrRef {
        let name = name.unwrap_or_else(|| self.func.upgrade().unwrap().next());
        let instr_ref = Rc::new(Instruction::GetElementPointer { name, ty, base, indices });
        self.instructions.borrow_mut().push(Rc::clone(&instr_ref));
        instr_ref
    }

    fn emit(&self) -> String {
        let mut s = format!("{}:\n", self.label);
        for instr in self.instructions.borrow().iter() {
            s += "  ";
            s += instr.emit().as_str();
        }
        s += "  ";
        s += self.terminator.borrow().as_ref().unwrap().emit().as_str();
        s
    }

    fn emit_ref(&self) -> String {
        format!("label %{}", self.label)
    }
}

pub type InstrRef = Rc<Instruction>;
pub type ValueRef = Rc<dyn Value>;

pub enum GEPIndex {
    StructIndex(u32),
    ArrayIndex(ValueRef)
}

pub enum Instruction {
    Add { name: String, ret: TypeRef, left: ValueRef, right: ValueRef },
    Alloca { name: String, ty: TypeRef },
    Call { name: String, ret: TypeRef, func: ValueRef, args: Vec<ValueRef> },
    GetElementPointer { name: String, ty: TypeRef, base: ValueRef, indices: Vec<GEPIndex> }
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
            Instruction::Call { name, ret, func, args } => {
                let args: Vec<String> = args.iter().map(|a| a.ty().emit() + &a.emit_value()).collect();
                format!("%{name} = call {} {}({})\n", ret.emit(), func.emit_value(), args.join(", "))
            }
            Instruction::GetElementPointer { name, ty, base, indices } => {
                let mut rendered_indices = vec![];
                for index in indices {
                    match index {
                        GEPIndex::StructIndex(value) => {
                            rendered_indices.push(format!("i32 {value}"))
                        }
                        GEPIndex::ArrayIndex(value) => {
                            rendered_indices.push(format!("{} {}", value.ty().emit(), value.emit_value()))
                        }
                    }
                }
                format!("${name} = getelementptr {} {}, {}", ty.emit(), base.emit_value(), rendered_indices.join(", "))
            }
        }
    }

    pub fn as_value(self: Rc<Instruction>) -> ValueRef {
        Rc::new(self)
    }
}

impl Value for Rc<Instruction> {
    fn emit_value(&self) -> String {
        match self.as_ref() {
            Instruction::Add { name, .. } => format!("%{}", name),
            Instruction::Alloca { name, .. } => format!("%{name}"),
            Instruction::Call { name, .. } => format!("%{name}"),
            Instruction::GetElementPointer { name, .. } => format!("%{name}")
        }
    }

    fn ty(&self) -> TypeRef {
        match self.as_ref() {
            Instruction::Add { ret, .. } => Rc::clone(ret),
            Instruction::Alloca { .. } => Types::ptr(),
            Instruction::Call { ret, .. } => Rc::clone(ret),
            Instruction::GetElementPointer { ty, indices, .. } => {
                let mut curr = Rc::clone(ty);
                for index in &indices[1..] {
                    match index {
                        GEPIndex::StructIndex(value) => {
                            curr = curr.type_at_index(Some(*value as usize));
                        }
                        GEPIndex::ArrayIndex(_) => {
                            curr = curr.type_at_index(None);
                        }
                    }
                }
                return curr;
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

pub struct NamedStruct {
    name: String,
    fields: RefCell<Option<Vec<TypeRef>>>,
}

impl NamedStruct {
    pub fn set_fields(&self, fields: Vec<TypeRef>) {
        *self.fields.borrow_mut() = Some(fields);
    }

    pub fn as_type_ref(self: &Rc<NamedStruct>) -> TypeRef {
        Rc::new(Type::NamedStruct(self.clone()))
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
}

#[cfg(test)]
mod test {
    use crate::llvm::{Constant, Module, Parameter, Type, Types};

    #[test]
    fn test_create_module() {
        let _ = Module::new(String::from("test"));
    }

    #[test]
    fn test_types() {
        let mut m = Module::new(String::from("test"));
        m.types.add_struct("<test>".into()).set_fields(vec![Types::int(32)]);

        m.emit();
    }

    #[test]
    fn test_function() {
        let mut m = Module::new(String::from("test"));

        let f = m.add_function("main", Types::int(32), vec![]);
        let b = f.add_block("entry".into());
        b.ret(Types::int_constant(32, 0).to_value());

        print!("{}", m.emit());
    }
}
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::{Rc, Weak};
use indexmap::IndexMap;

pub type FunctionRef = Rc<Function>;

pub type StructRef = Rc<NamedStruct>;

pub type TypeRef = Box<Type>;

pub enum Type {
    Void,
    Integer(u8),
    Array(TypeRef, usize),
    NamedStruct(StructRef),
    Struct(Vec<TypeRef>)
}

impl Type {
    pub fn emit(&self) -> String {
        match self {
            Type::Void => "void".into(),
            Type::Integer(bits) => format!("i{}", bits),
            Type::Array(t, count) => format!("[{} x {}]", t.emit(), count),
            Type::NamedStruct(s) => format!("%{}", s.name),
            Type::Struct(items) => {
                let items: Vec<String> = items.iter().map(|t| t.emit()).collect();
                format!("{{ {} }}", items.join(", "))
            }
        }
    }
}

pub struct Types {
    unique: u32,
    named_structs: IndexMap<String, Rc<NamedStruct>>
}

impl Types {
    pub fn new() -> Types {
        Types {
            unique: 0,
            named_structs: IndexMap::new()
        }
    }

    pub fn add_struct(&mut self, name: Option<&str>) -> StructRef {
        let name: String = name.map_or_else(|| {
            let val = self.unique;
            self.unique += 1;
            format!("_struct_{:>08X}", val)
        }, String::from);
        let item = Rc::new(NamedStruct { name: name.clone(), fields: RefCell::new(None) });
        self.named_structs.insert(name, Rc::clone(&item));
        item
    }

    pub fn int(bits: u8) -> TypeRef {
        Box::new(Type::Integer(bits))
    }

    fn emit(&self) -> String {
        let mut types = String::new();
        for s in self.named_structs.values() {
            types += s.emit().as_str();
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
        let item = Rc::new(Function { name: name.into(), ret, parameters, blocks: RefCell::new(vec![]) });
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

trait Value {
    fn _emit_value(&self) -> String;
}

enum Constant {
    ZeroInitializer,
    Integer {
        bits: u8,
        value: u64
    },
    String {
        chars: Vec<u8>
    },
    Array {
        ty: TypeRef,
        items: Vec<Constant>,
    },
    Struct {
        items: Vec<Constant>,
    },
    Boolean(bool)
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
}

impl Function {
    pub fn add_block(&self, name: String) -> BasicBlockRef {
        let item = Rc::new(BasicBlock {
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
            format!("define {} @{}({}) {{\n{}}}\n", self.ret.emit(), self.name, arguments.join(", "), blocks)
        } else {
            format!("declare {} @{}({})\n", self.ret.emit(), self.name, arguments.join(", "))
        }
    }
}

pub struct Parameter {
    name: String,
    typ: TypeRef
}

impl Parameter {
    fn emit(&self) -> String {
        format!("{} %{}", self.typ.emit(), self.name)
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
            s += instr.emit().as_str();
        }
        s
    }
}

pub type InstrRef = Rc<Instruction>;

pub enum Instruction {
    Add { name: String, ret: TypeRef, left: InstrRef, right: InstrRef }
}

impl Instruction {
    fn emit(&self) -> String {
        match self {
            Instruction::Add { name, ret, left, right} => {
                format!("%{} = add {} {}, {}\n", name, ret.emit(), left._emit_value(), right._emit_value())
            }
        }
    }
}

impl Value for Instruction {
    fn _emit_value(&self) -> String {
        match self {
            Instruction::Add { name, .. } => format!("%{}", name)
        }
    }
}

pub enum Terminator {

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
        Box::new(Type::NamedStruct(self.clone()))
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
    use crate::llvm::{Module, Parameter, Type, Types};

    #[test]
    fn test_create_module() {
        let _ = Module::new(String::from("test"));
    }

    #[test]
    fn test_types() {
        let mut m = Module::new(String::from("test"));
        m.types.add_struct(None).set_fields(vec![Types::int(32)]);

        m.emit();
    }

    #[test]
    fn test_function() {
        let mut m = Module::new(String::from("test"));

        let f = m.add_function("main", Types::int(32), vec![]);
        let b = f.add_block("entry".into());


        print!("{}", m.emit());
    }
}
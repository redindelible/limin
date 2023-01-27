use std::collections::HashMap;
use crate::ast;
use crate::source::Loc;
use crate::error::{CompilerMessage, MessageLevel};

enum Ty {
    Unknown,
    TypeVar { name: String },
    Integer { bits: u8, signed: bool },
    Struct { name: String, ty_args: Vec<Box<Ty>>, fields: HashMap<String, Box<Ty>> }
}

pub struct ValueDeclData<'ast> {
    name: String,
    ty: Ty,
    loc: Loc<'ast>
}

impl<'ast> ValueDeclData<'ast> {
    pub fn new(name: String, loc: Loc<'ast>) -> ValueDeclData<'ast> {
        ValueDeclData {
            name,
            ty: Ty::Unknown,
            loc
        }
    }
}

pub struct ValueData<'ast> {
    ty: Ty,
    loc: Loc<'ast>
}

impl<'ast> ValueData<'ast> {
    pub fn new(loc: Loc<'ast>) -> ValueData<'ast> {
        ValueData {
            ty: Ty::Unknown,
            loc
        }
    }
}

pub struct TypeDeclData<'ast> {
    name: String,
    ty: Ty,
    loc: Option<Loc<'ast>>
}

impl<'ast> TypeDeclData<'ast> {
    pub fn new(name: String, loc: Loc<'ast>) -> TypeDeclData<'ast> {
        TypeDeclData {
            name,
            ty: Ty::Unknown,
            loc: Some(loc)
        }
    }

    fn new_builtin(name: &str, ty: Ty) -> TypeDeclData<'ast> {
        TypeDeclData {
            name: name.to_string(),
            ty,
            loc: None
        }
    }
}

pub struct TypeData<'ast> {
    ty: Ty,
    loc: Loc<'ast>
}

impl<'ast> TypeData<'ast> {
    pub fn new(loc: Loc<'ast>) -> TypeData<'ast> {
        TypeData {
            ty: Ty::Unknown,
            loc
        }
    }
}

pub struct NamespaceDeclData<'ast> {
    name: String,
    loc: Option<Loc<'ast>>,

    values: HashMap<String, &'ast ValueDeclData<'ast>>,
    types: HashMap<String, &'ast TypeDeclData<'ast>>,
    namespaces: HashMap<String, &'ast mut NamespaceDeclData<'ast>>
}

impl<'ast> NamespaceDeclData<'ast> {
    pub fn new(name: String) -> NamespaceDeclData<'ast> {
        NamespaceDeclData {
            name,
            loc: None,
            values: HashMap::new(),
            types: HashMap::new(),
            namespaces: HashMap::new(),
        }
    }

    pub fn new_located(name: String, loc: Loc<'ast>) -> NamespaceDeclData<'ast> {
        NamespaceDeclData {
            name,
            loc: Some(loc),
            values: HashMap::new(),
            types: HashMap::new(),
            namespaces: HashMap::new(),
        }
    }

    fn add_value(&'ast mut self, value: &'ast mut ValueDeclData<'ast>) -> Result<(), CompilerMessage> {
        match self.values.get(&value.name) {
            Some(&val) => {
                let mut message = CompilerMessage::new_located(
                    MessageLevel::Error,
                    format!("Name '{}' already exists for a value in this scope.", value.name),
                    value.loc
                );
                message.add_message(CompilerMessage::new_located(
                    MessageLevel::Note,
                    "Previously defined here:".to_string(),
                    val.loc
                ));
                Err(message)
            },
            None => {
                self.values.insert(value.name.clone(), value);
                Ok(())
            }
        }
    }

    fn add_type(&'ast mut self, value: &'ast TypeDeclData<'ast>) -> Result<(), CompilerMessage> {
        match self.types.get(&value.name) {
            Some(&typ @ TypeDeclData{loc: Some(loc), ..})=> {
                let mut message = CompilerMessage::new_located(
                    MessageLevel::Error,
                    format!("Name '{}' already exists for a type in this scope.", typ.name),
                    *loc
                );
                message.add_message(CompilerMessage::new_located(
                    MessageLevel::Note,
                    "Previously defined here:".to_string(),
                    *loc
                ));
                Err(message)
            },
            Some(&typ @ TypeDeclData{loc: None, ..})=> {
                let message = CompilerMessage::new_no_loc(
                    MessageLevel::Error,
                    format!("Name '{}' would shadow the builtin type.", typ.name)
                );
                Err(message)
            },
            None => {
                self.types.insert(value.name.clone(), value);
                Ok(())
            }
        }
    }
}

pub struct NamespaceData<'ast> {
    loc: Loc<'ast>
}

impl<'ast> NamespaceData<'ast> {
    pub fn new(loc: Loc<'ast>) -> Self {
        Self {
            loc
        }
    }
}

pub struct Builtins<'ast> {
    u32_ty: TypeDeclData<'ast>,
}

impl<'ast> Builtins<'ast> {
    pub fn new() -> Self {
        Builtins {
            u32_ty: TypeDeclData::new_builtin("u32", Ty::Integer {bits: 32, signed: false})
        }
    }
}

pub struct Program<'ast> {
    root_ns: NamespaceDeclData<'ast>,
    u32_ty: TypeDeclData<'ast>,
}

impl<'ast> Program<'ast> {
    pub fn new() -> Self {
        let mut program = Program {
            root_ns: NamespaceDeclData::new("<root>".to_string()),
            u32_ty: TypeDeclData::new_builtin("u32", Ty::Integer {bits: 32, signed: false})
        };
        return program;
    }

    pub fn add_u32(&'ast mut self) {
        self.root_ns.add_type(&self.u32_ty);
    }
}


pub struct InferenceEngine<'ast> {
    ast: &'ast ast::AST,
    namespace_data: Vec<&'ast mut NamespaceDeclData<'ast>>
}

impl<'ast> InferenceEngine<'ast> {
    pub fn new(ast: &'ast ast::AST) -> Self {
        let mut engine = InferenceEngine {
            ast,
            namespaces: vec![]
        };
        return engine;
    }

    fn curr_ns(&'ast mut self) -> &'ast mut NamespaceDeclData<'ast> {
        if self.namespaces.is_empty() {
            &mut self.program.root_ns
        } else {
            *self.namespaces.last_mut().unwrap()
        }
    }

    fn add_value(&'ast mut self, value: &'ast mut ValueDeclData<'ast>) -> Result<(), CompilerMessage> {
        self.curr_ns().add_value(value)
    }

    fn collect_types(&mut self) {

    }
}
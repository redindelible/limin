use std::collections::HashMap;
use std::path::PathBuf;
use crate::source::{HasLoc, Location};

#[derive(Debug, Eq, PartialEq)]
pub struct AST<'a> {
    pub name: String,
    pub files: HashMap<PathBuf, File<'a>>
}

impl AST<'_> {
    pub fn from_files(name: String, files: Vec<File>) -> AST {
        let mut map = HashMap::new();
        for file in files {
            map.insert(file.path.clone(), file);
        }
        AST { name, files: map }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct File<'a> {
    pub path: PathBuf,
    pub top_levels: Vec<TopLevel<'a>>
}

impl<'a> File<'a> {
    pub fn iter_structs(&self) -> impl Iterator<Item = &Struct<'a>>  {
        self.top_levels.iter().filter_map(|t|
            if let TopLevel::Struct(struct_) = t {
                Some(struct_)
            } else {
                None
            }
        )
    }

    pub fn iter_functions(&self) -> impl Iterator<Item = &Function<'a>>  {
        self.top_levels.iter().filter_map(|t|
            if let TopLevel::Function(function) = t {
                Some(function)
            } else {
                None
            }
        )
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum TopLevel<'a> {
    Function(Function<'a>),
    Struct(Struct<'a>)
}

#[derive(Debug, Eq, PartialEq)]
pub struct Function<'a>  {
    pub name: String,
    pub type_parameters: Vec<TypeParameter<'a>>,
    pub parameters: Vec<Parameter<'a>>,
    pub return_type: Option<Box<Type<'a>>>,
    pub body: Block<'a>,
    pub loc: Location<'a>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct TypeParameter<'a> {
    pub name: String,
    pub bound: Option<Box<Type<'a>>>,
    pub loc: Location<'a>
}

#[derive(Debug, Eq, PartialEq)]
pub struct Struct<'a> {
    pub name: String,
    pub type_params: Vec<TypeParameter<'a>>,
    pub super_struct: Option<(String, Location<'a>)>,
    pub items: Vec<StructItem<'a>>,
    pub loc: Location<'a>
}

#[derive(Debug, Eq, PartialEq)]
pub enum StructItem<'a> {
    Field { name: String, typ: Box<Type<'a>>, loc: Location<'a> }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Parameter<'a> {
    pub name: String,
    pub typ: Box<Type<'a>>,
    pub loc: Location<'a>
}

#[derive(Debug, Eq, PartialEq)]
pub enum Stmt<'a> {
    Decl { name: String, typ: Option<Box<Type<'a>>>, value: Box<Expr<'a>>, loc: Location<'a> },
    Return { value: Box<Expr<'a>>, loc: Location<'a> },
    Expr { expr: Box<Expr<'a>>, loc: Location<'a> }
}


#[derive(Debug, Eq, PartialEq)]
pub enum BinOp {
    LessThan,
    GreaterThan
}

#[derive(Debug, Eq, PartialEq)]
pub struct Block<'a> {
    pub stmts: Vec<Box<Stmt<'a>>>,
    pub trailing_expr: Option<Box<Expr<'a>>>,
    pub loc: Location<'a>
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr<'a> {
    Name { name: String, loc: Location<'a> },
    GetAttr { obj: Box<Expr<'a>>, attr: String, loc: Location<'a> },
    BinOp { left: Box<Expr<'a>>, op: BinOp, right: Box<Expr<'a>>, loc: Location<'a> },
    Call { callee: Box<Expr<'a>>, arguments: Vec<Expr<'a>>, loc: Location<'a> },
    GenericCall { callee: Box<Expr<'a>>, generic_arguments: Vec<Type<'a>>, arguments: Vec<Expr<'a>>, loc: Location<'a> },
    Integer { number: u64, loc: Location<'a> },
    Bool { value: bool, loc: Location<'a> },
    Block(Block<'a>),
    New { struct_: String, type_args: Option<Vec<Type<'a>>>, fields: Vec<NewArgument<'a>>, loc: Location<'a> },
    IfElse { cond: Box<Expr<'a>>, then_do: Box<Expr<'a>>, else_do: Box<Expr<'a>>, loc: Location<'a> },
    Closure { parameters: Vec<ClosureParameter<'a>>, body: Box<Expr<'a>>, loc: Location<'a> }
}

#[derive(Debug, Eq, PartialEq)]
pub struct NewArgument<'a> {
    pub field_name: String,
    pub name_loc: Location<'a>,
    pub argument: Box<Expr<'a>>
}

#[derive(Debug, Eq, PartialEq)]
pub struct ClosureParameter<'a> {
    pub name: String,
    pub typ: Option<Type<'a>>,
    pub loc: Location<'a>
}

impl<'a> HasLoc<'a> for Expr<'a> {
    fn loc(&self) -> Location<'a> {
        match self {
            Expr::Name { loc, .. } => *loc,
            Expr::GetAttr { loc, .. } => *loc,
            Expr::BinOp { loc, .. } => *loc,
            Expr::Call { loc, .. } => *loc,
            Expr::GenericCall { loc, .. } => *loc,
            Expr::Integer { loc, .. } => *loc,
            Expr::Block(Block{ loc, .. }) => *loc,
            Expr::New { loc, .. } => *loc,
            Expr::Bool { loc, .. } => *loc,
            Expr::IfElse { loc, .. } => *loc,
            Expr::Closure { loc, .. } => *loc,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Type<'a> {
    Name { name: String, loc: Location<'a> },
    Function { parameters: Vec<Type<'a>>, ret: Box<Type<'a>>, loc: Location<'a> },
    Generic { name: String, type_args: Vec<Type<'a>>, loc: Location<'a> }
}

impl<'s> HasLoc<'s> for Type<'s> {
    fn loc(&self) -> Location<'s> {
        match self {
            Type::Name { loc, .. } => *loc,
            Type::Function { loc, .. } => *loc,
            Type::Generic { loc, .. } => *loc
        }
    }
}
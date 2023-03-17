use std::collections::HashMap;
use std::path::PathBuf;
use crate::source::{HasLoc, Location};

#[derive(Debug, Eq, PartialEq)]
pub struct AST<'a> {
    pub files: HashMap<PathBuf, File<'a>>
}

#[derive(Debug, Eq, PartialEq)]
pub struct File<'a> {
    pub path: PathBuf,
    pub top_levels: Vec<TopLevel<'a>>
}

impl<'a> File<'a> {
    pub fn iter_structs(&self) -> impl Iterator<Item = (&String, &Vec<StructItem<'a>>)>  {
        self.top_levels.iter().filter_map(|t|
            if let TopLevel::Struct { name, items} = t {
                Some((name, items))
            } else {
                None
            }
        )
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum TopLevel<'a> {
    Function { name: String, parameters: Vec<Box<Parameter<'a>>>, return_type: Option<Box<Type<'a>>>, body: Box<Expr<'a>> },
    Struct { name: String, items: Vec<StructItem<'a>> }
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
    Decl { name: String, typ: Box<Type<'a>>, value: Box<Expr<'a>> },
    Return { value: Box<Expr<'a>> },
    Expr { expr: Box<Expr<'a>> }
}


#[derive(Debug, Eq, PartialEq)]
pub enum BinOp {
    LessThan,
    GreaterThan
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr<'a> {
    Name { name: String, loc: Location<'a> },
    BinOp { left: Box<Expr<'a>>, op: BinOp, right: Box<Expr<'a>>, loc: Location<'a> },
    Call { callee: Box<Expr<'a>>, arguments: Vec<Box<Expr<'a>>>, loc: Location<'a> },
    GenericCall { callee: Box<Expr<'a>>, generic_arguments: Vec<Box<Type<'a>>>, arguments: Vec<Box<Expr<'a>>>, loc: Location<'a> },
    Integer { number: u64, loc: Location<'a> },
    Block { stmts: Vec<Box<Stmt<'a>>>, trailing_semicolon: bool, loc: Location<'a> }
}

impl<'a> HasLoc<'a> for Expr<'a> {
    fn loc(&self) -> Location<'a> {
        match self {
            Expr::Name { loc, .. } => *loc,
            Expr::BinOp { loc, .. } => *loc,
            Expr::Call { loc, .. } => *loc,
            Expr::GenericCall { loc, .. } => *loc,
            Expr::Integer { loc, .. } => *loc,
            Expr::Block { loc, .. } => *loc
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Type<'a> {
    Name { name: String, loc: Location<'a> }
}

impl<'s> HasLoc<'s> for Type<'s> {
    fn loc(&self) -> Location<'s> {
        match self {
            Type::Name { loc, .. } => *loc
        }
    }
}
use crate::source::Location;

#[derive(Debug, Eq, PartialEq)]
pub struct AST<'a> {
    pub files: Vec<File<'a>>
}

#[derive(Debug, Eq, PartialEq)]
pub struct File<'a> {
    pub top_levels: Vec<TopLevel<'a>>
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
    BinOp { left: Box<Expr<'a>>, op: BinOp, right: Box<Expr<'a>> },
    Call { callee: Box<Expr<'a>>, arguments: Vec<Box<Expr<'a>>> },
    GenericCall { callee: Box<Expr<'a>>, generic_arguments: Vec<Box<Type<'a>>>, arguments: Vec<Box<Expr<'a>>> },
    Integer { number: u64, loc: Location<'a> },
    Block { stmts: Vec<Box<Stmt<'a>>>, trailing_semicolon: bool }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Type<'a> {
    Name { name: String, loc: Location<'a> }
}

impl<'s> Type<'s> {
    pub fn loc(&self) -> Location<'s> {
        match self {
            Self::Name { loc, .. } => *loc
        }
    }
}
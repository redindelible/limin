use crate::source::Location;

#[derive(Debug, Eq, PartialEq)]
pub enum TopLevel<'a> {
    Function { name: String, parameters: Vec<Box<Parameter<'a>>>, return_type: Option<Box<Type<'a>>>, body: Box<Expr<'a>> }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Parameter<'a> {
    pub name: String,
    pub typ: Box<Type<'a>>
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
    Integer { number: u64, loc: Location<'a> }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Type<'a> {
    Name { name: String, loc: Location<'a> }
}
use crate::source::Location;

pub enum TopLevel {

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
    GenericCall { callee: Box<Expr<'a>>, generic_arguments: Vec<Box<Type<'a>>>, arguments: Vec<Box<Expr<'a>>> }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Type<'a> {
    Name { name: String, loc: Location<'a> }
}
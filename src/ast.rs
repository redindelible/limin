use crate::source::Location;

#[derive(Debug)]
pub enum Expr<'a> {
    Name(ExprNameData<'a>)
}

#[derive(Debug)]
pub struct ExprNameData<'a> {
    pub name: String,
    pub loc: Location<'a>
}
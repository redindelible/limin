use crate::source::Location;

pub enum Expr<'a> {
    Name(ExprNameData<'a>)
}

pub struct ExprNameData<'a> {
    pub name: String,
    pub loc: Location<'a>
}
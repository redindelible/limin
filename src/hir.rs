use std::collections::HashMap;
use typed_arena::Arena;
use crate::source::Location;

pub struct NameDecl<'s> {
    pub name: String,
    pub typ: Type,
    pub decl: Location<'s>
}

pub enum Type {
    Integer { bits: u8 }
}

pub struct HIR<'s> {
    pub names: Arena<NameDecl<'s>>
}

pub struct Function<'ir> {
    pub name: String,
    pub body: Expr<'ir>
}

pub enum Expr<'ir> {
    Name { decl: &'ir NameDecl<'ir> }
}
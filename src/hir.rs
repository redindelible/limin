use std::collections::HashMap;
use typed_arena::Arena;
use crate::source::Location;

pub struct NameDecl<'s> {
    name: String,
    typ: Type<'s>,
    decl: Location<'s>
}

pub enum Type<'s> {
    Integer { bits: u8 }
}

pub struct HIR<'s> {
    names: Arena<NameDecl<'s>>
}

pub enum Expr<'ir> {
    Name { decl: &'ir NameDecl<'ir> }
}
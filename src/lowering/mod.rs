mod hir;
mod type_check;
mod mir;
mod monomorphize;
mod lower;

pub use crate::lowering::type_check::TypeCheckError;
use crate::parsing::ast;
use crate::emit::lir;

pub fn lower(ast: ast::AST) -> Result<lir::LIR, Vec<TypeCheckError>> {
    let types_resolved = type_check::resolve_types(ast)?;
    let monomorphized = monomorphize::monomorphize(types_resolved);
    let lowered = lower::lower(monomorphized);
    Ok(lowered)
}

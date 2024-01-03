use crate::lowering::type_check as tc;

pub(super) fn collect_types<'a, 'b>(_: &mut tc::TypeCheck<'a>, collected: tc::CollectedTypes<'a, 'b>) -> tc::CollectedTypes<'a, 'b> {
    collected
}
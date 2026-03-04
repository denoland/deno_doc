use crate::Declaration;
use crate::html::DocNodeWithContext;
use crate::html::render_context::RenderContext;
use crate::html::util::*;

pub(crate) fn render_variable(
  ctx: &RenderContext,
  symbol: &DocNodeWithContext,
  decl: &Declaration,
) -> Vec<SectionCtx> {
  let variable_def = decl.variable_def().unwrap();

  let Some(ts_type) = &variable_def.ts_type else {
    return vec![];
  };

  let var_diff = ctx.ctx.diff.as_ref().and_then(|diff_index| {
    diff_index
      .get_def_diff(
        &symbol.origin.specifier,
        symbol.get_name(),
        decl.def.to_kind(),
      )
      .and_then(|d| d.as_variable())
  });

  let id = IdBuilder::new(ctx)
    .kind(IdKind::Variable)
    .name(symbol.get_qualified_name())
    .build();

  super::render_type_def_sections(
    ctx,
    symbol.get_qualified_name(),
    ts_type,
    var_diff.and_then(|d| d.ts_type_change.as_ref()),
    id,
    "Type",
    &decl.location,
  )
}

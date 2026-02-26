use crate::html::DocNodeWithContext;
use crate::html::render_context::RenderContext;
use crate::html::util::*;

pub(crate) fn render_variable(
  ctx: &RenderContext,
  doc_node: &DocNodeWithContext,
  name: &str,
) -> Vec<SectionCtx> {
  let variable_def = doc_node.variable_def().unwrap();

  let Some(ts_type) = &variable_def.ts_type else {
    return vec![];
  };

  let var_diff = ctx.ctx.diff.as_ref().and_then(|diff_index| {
    diff_index
      .get_def_diff(
        &doc_node.origin.specifier,
        doc_node.get_name(),
        doc_node.def.to_kind(),
      )
      .and_then(|d| d.as_variable())
  });

  let id = IdBuilder::new(ctx)
    .kind(IdKind::Variable)
    .name(doc_node.get_qualified_name())
    .build();

  super::render_type_def_sections(
    ctx,
    name,
    ts_type,
    var_diff.and_then(|d| d.ts_type_change.as_ref()),
    id,
    "Type",
    &doc_node.location,
  )
}

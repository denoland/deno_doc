use crate::html::render_context::RenderContext;
use crate::html::types::render_type_def;
use crate::html::util::*;
use crate::html::DocNodeWithContext;
use std::collections::HashSet;

pub(crate) fn render_variable(
  ctx: &RenderContext,
  doc_node: &DocNodeWithContext,
) -> Vec<SectionCtx> {
  let variable_def = doc_node.variable_def.as_ref().unwrap();

  if variable_def.ts_type.is_none() {
    return vec![];
  }

  let id = name_to_id("variable", doc_node.get_name());

  vec![SectionCtx::new(
    "Type",
    SectionContentCtx::DocEntry(vec![DocEntryCtx::new(
      ctx,
      &id,
      "",
      None,
      &render_type_def(ctx, variable_def.ts_type.as_ref().unwrap()),
      HashSet::new(),
      None,
      &doc_node.location,
    )]),
  )]
}

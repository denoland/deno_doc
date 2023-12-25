use crate::html::jsdoc::SectionCtx;
use crate::html::jsdoc::{DocEntryCtx, SectionContentCtx};
use crate::html::types::render_type_def;
use crate::html::util::*;

pub(crate) fn render_variable(
  ctx: &RenderContext,
  doc_node: &crate::DocNode,
) -> String {
  let variable_def = doc_node.variable_def.as_ref().unwrap();

  if variable_def.ts_type.is_none() {
    return String::new();
  }

  let id = name_to_id("variable", &doc_node.name);

  ctx.render(
    "section",
    &SectionCtx {
      title: "Type",
      content: SectionContentCtx::DocEntry(vec![DocEntryCtx::new(
        ctx,
        &id,
        "",
        &render_type_def(ctx, variable_def.ts_type.as_ref().unwrap()),
        None,
      )]),
    },
  )
}

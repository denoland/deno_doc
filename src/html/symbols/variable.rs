use crate::html::jsdoc::render_doc_entry;
use crate::html::types::render_type_def;
use crate::html::util::*;
use serde_json::json;

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
    &json!({
      "title": "type",
      "content": render_doc_entry(
        ctx,
        &id,
        "",
        &render_type_def(ctx, variable_def.ts_type.as_ref().unwrap()),
        None,
      )
    }),
  )
}

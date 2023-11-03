use crate::html::types::render_type_def;
use crate::html::util::*;
use serde_json::json;

use super::GenerateCtx;

pub(super) fn render_variable(
  ctx: &GenerateCtx,
  doc_node: &crate::DocNode,
  render_ctx: &RenderContext,
) -> String {
  let variable_def = doc_node.variable_def.as_ref().unwrap();

  if variable_def.ts_type.is_none() {
    return String::new();
  }

  let id = name_to_id("variable", &doc_node.name);

  ctx.render("nodes/variable.html", &json!({
    "docs": super::jsdoc::render_docs(ctx, &doc_node.js_doc, true, false, render_ctx),
    "doc_entry": {
      "title": "type",
      "content": render_doc_entry(
        ctx,
        &id,
        "",
        &render_type_def(variable_def.ts_type.as_ref().unwrap(), render_ctx),
        None,
        render_ctx,
      )
    }
  }))
}

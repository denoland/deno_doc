use crate::html::types::render_type_def;
use crate::html::util::*;

pub fn render_variable(
  doc_node: &crate::DocNode,
  ctx: &RenderContext,
) -> String {
  let variable_def = doc_node.variable_def.as_ref().unwrap();

  if variable_def.ts_type.is_none() {
    return String::new();
  }

  let id = name_to_id("variable", &doc_node.name);

  format!(
    r#"<div class="doc_block_items">{}{}</div>"#,
    super::jsdoc::render_docs(&doc_node.js_doc, true, false, ctx),
    section(
      "type",
      &doc_entry(
        &id,
        "",
        &render_type_def(variable_def.ts_type.as_ref().unwrap(), ctx),
        None,
        ctx,
      )
    )
  )
}

use crate::html::types::render_type_def;
use crate::html::util::*;

use super::GenerateCtx;

pub(super) fn render_enum(
  _ctx: &GenerateCtx,
  doc_node: &crate::DocNode,
  render_ctx: &RenderContext,
) -> String {
  let mut members = doc_node.enum_def.as_ref().unwrap().members.clone();

  members.sort_by(|a, b| a.name.cmp(&b.name));

  let items = members
    .into_iter()
    .map(|member| {
      let id =
        name_to_id("enum", &format!("{}_{}", &doc_node.name, &member.name));
      doc_entry(
        &id,
        &member.name,
        &member
          .init
          .as_ref()
          .map(|init| format!(" = {}", render_type_def(init, render_ctx)))
          .unwrap_or_default(),
        member.js_doc.doc.as_deref(),
        render_ctx,
      )
    })
    .collect::<String>();

  format!(
    r#"<div class="doc_block_items">{}{}</div>"#,
    super::jsdoc::render_docs(&doc_node.js_doc, true, false, render_ctx),
    section("Members", &items),
  )
}

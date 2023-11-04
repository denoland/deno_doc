use crate::html::jsdoc::render_doc_entry;
use crate::html::types::render_type_def;
use crate::html::util::*;
use crate::html::GenerateCtx;
use serde_json::json;

pub(crate) fn render_enum(
  ctx: &GenerateCtx,
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
      render_doc_entry(
        ctx,
        render_ctx,
        &id,
        &member.name,
        &member
          .init
          .as_ref()
          .map(|init| format!(" = {}", render_type_def(ctx, init, render_ctx)))
          .unwrap_or_default(),
        member.js_doc.doc.as_deref(),
      )
    })
    .collect::<String>();

  ctx.render(
    "section.html",
    &json!({ "title": "Members", "content": &items }),
  )
}

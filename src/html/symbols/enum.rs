use crate::html::render_context::RenderContext;
use crate::html::types::render_type_def;
use crate::html::util::*;
use crate::html::DocNodeWithContext;

pub(crate) fn render_enum(
  render_ctx: &RenderContext,
  doc_node: &DocNodeWithContext,
) -> Vec<SectionCtx> {
  let mut members = doc_node.enum_def.as_ref().unwrap().members.clone();

  members.sort_by(|a, b| a.name.cmp(&b.name));

  let items = members
    .into_iter()
    .map(|member| {
      let id = name_to_id(
        "enum",
        &format!("{}_{}", doc_node.get_name(), &member.name),
      );

      let tags = Tag::from_js_doc(&member.js_doc);

      DocEntryCtx::new(
        render_ctx,
        &id,
        &html_escape::encode_safe(&member.name),
        None,
        &member
          .init
          .as_ref()
          .map(|init| format!(" = {}", render_type_def(render_ctx, init)))
          .unwrap_or_default(),
        tags,
        member.js_doc.doc.as_deref(),
        &member.location,
      )
    })
    .collect::<Vec<DocEntryCtx>>();

  vec![SectionCtx {
    title: "Members",
    content: SectionContentCtx::DocEntry(items),
  }]
}

use crate::html::render_context::RenderContext;
use crate::html::types::render_type_def;
use crate::html::util::*;
use crate::html::DocNodeWithContext;

pub(crate) fn render_enum(
  render_ctx: &RenderContext,
  doc_node: &DocNodeWithContext,
) -> Vec<SectionCtx> {
  let mut members = doc_node.enum_def().unwrap().members.clone();

  members.sort_by(|a, b| a.name.cmp(&b.name));

  let items = members
    .into_iter()
    .map(|member| {
      let id = IdBuilder::new(render_ctx.ctx)
        .kind(IdKind::Enum)
        .name(doc_node.get_name())
        .component(&member.name)
        .build();

      let tags = Tag::from_js_doc(&member.js_doc);

      DocEntryCtx::new(
        render_ctx,
        id,
        Some(html_escape::encode_text(&member.name).into_owned()),
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

  vec![SectionCtx::new(
    render_ctx,
    "Members",
    SectionContentCtx::DocEntry(items),
  )]
}

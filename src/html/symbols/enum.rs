use crate::r#enum::EnumMemberDef;
use crate::html::DiffStatus;
use crate::html::DocNodeWithContext;
use crate::html::render_context::RenderContext;
use crate::html::types::render_type_def;
use crate::html::util::*;
use crate::js_doc::JsDocTag;

pub(crate) fn render_enum(
  render_ctx: &RenderContext,
  doc_node: &DocNodeWithContext,
) -> Vec<SectionCtx> {
  let enum_def = doc_node.enum_def().unwrap();

  let enum_diff = render_ctx.ctx.diff.as_ref().and_then(|diff_index| {
    diff_index
      .get_def_diff(
        &doc_node.origin.specifier,
        doc_node.get_name(),
        doc_node.def.to_kind(),
      )
      .and_then(|d| d.as_enum())
  });

  let mut members = enum_def.members.clone();
  members.sort_by(|a, b| {
    let is_deprecated = |m: &EnumMemberDef| {
      m.js_doc
        .tags
        .iter()
        .any(|t| matches!(t, JsDocTag::Deprecated { .. }))
    };

    is_deprecated(a)
      .cmp(&is_deprecated(b))
      .then_with(|| a.name.cmp(&b.name))
  });

  let mut items = members
    .into_iter()
    .map(|member| {
      let id = IdBuilder::new(render_ctx)
        .kind(IdKind::Enum)
        .name(doc_node.get_name())
        .name(&member.name)
        .build();

      let tags = Tag::from_js_doc(&member.js_doc);

      let diff_status = if let Some(diff) = enum_diff {
        if diff.added_members.iter().any(|m| m.name == member.name) {
          Some(DiffStatus::Added)
        } else if diff.modified_members.iter().any(|m| m.name == member.name) {
          Some(DiffStatus::Modified)
        } else {
          None
        }
      } else {
        None
      };

      let (old_content, old_tags, js_doc_changed) =
        if matches!(diff_status, Some(DiffStatus::Modified)) {
          let member_diff = enum_diff.and_then(|d| {
            d.modified_members.iter().find(|m| m.name == member.name)
          });
          let old_content = member_diff
            .and_then(|md| md.init_change.as_ref())
            .map(|tc| format!(" = {}", render_type_def(render_ctx, &tc.old)));
          let old_tags =
            Some(super::compute_old_tags(&tags, None, None, None, None));
          let js_doc_changed =
            member_diff.and_then(|md| md.js_doc_change.as_ref().map(|_| true));
          (old_content, old_tags, js_doc_changed)
        } else {
          (None, None, None)
        };

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
        diff_status,
        old_content,
        old_tags,
        js_doc_changed,
      )
    })
    .collect::<Vec<DocEntryCtx>>();

  // Inject removed members
  if let Some(enum_diff) = enum_diff {
    for removed_member in &enum_diff.removed_members {
      let id = IdBuilder::new(render_ctx)
        .kind(IdKind::Enum)
        .name(doc_node.get_name())
        .name(&removed_member.name)
        .build();

      let tags = Tag::from_js_doc(&removed_member.js_doc);

      items.push(DocEntryCtx::removed(
        render_ctx,
        id,
        Some(html_escape::encode_text(&removed_member.name).into_owned()),
        None,
        &removed_member
          .init
          .as_ref()
          .map(|init| format!(" = {}", render_type_def(render_ctx, init)))
          .unwrap_or_default(),
        tags,
        removed_member.js_doc.doc.as_deref(),
        &removed_member.location,
      ));
    }
  }

  vec![SectionCtx::new(
    render_ctx,
    "Members",
    SectionContentCtx::DocEntry(items),
  )]
}

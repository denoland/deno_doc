use crate::diff::EnumDiff;
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

  // Extract EnumDiff if available
  let enum_diff = render_ctx.ctx.diff.as_ref().and_then(|diff_index| {
    let info = diff_index.get_node_diff(
      &doc_node.origin.specifier,
      doc_node.get_name(),
      doc_node.def.to_kind(),
    )?;
    let node_diff = info.diff.as_ref()?;
    if let crate::diff::DocNodeDefDiff::Enum(enum_diff) =
      node_diff.def_changes.as_ref()?
    {
      Some(enum_diff)
    } else {
      None
    }
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

      let diff_status = get_member_diff_status(enum_diff, &member.name);

      let (old_content, js_doc_changed) =
        if matches!(diff_status, Some(DiffStatus::Modified)) {
          let member_diff = enum_diff.and_then(|d| {
            d.modified_members
              .iter()
              .find(|m| m.name == member.name)
          });
          let old_content = member_diff
            .and_then(|md| md.init_change.as_ref())
            .map(|tc| format!(" = {}", render_type_def(render_ctx, &tc.old)));
          let js_doc_changed =
            member_diff.and_then(|md| md.js_doc_change.as_ref().map(|_| true));
          (old_content, js_doc_changed)
        } else {
          (None, None)
        };

      DocEntryCtx::new_with_diff(
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
        None,
        old_content,
        None,
        js_doc_changed,
      )
    })
    .collect::<Vec<DocEntryCtx>>();

  // Inject removed members
  if let Some(diff) = enum_diff {
    inject_removed_members(render_ctx, doc_node.get_name(), diff, &mut items);
  }

  vec![SectionCtx::new(
    render_ctx,
    "Members",
    SectionContentCtx::DocEntry(items),
  )]
}

fn get_member_diff_status(
  enum_diff: Option<&EnumDiff>,
  name: &str,
) -> Option<DiffStatus> {
  let diff = enum_diff?;

  if diff.added_members.iter().any(|m| &*m.name == name) {
    return Some(DiffStatus::Added);
  }
  if diff.modified_members.iter().any(|m| &*m.name == name) {
    return Some(DiffStatus::Modified);
  }
  None
}

fn inject_removed_members(
  render_ctx: &RenderContext,
  enum_name: &str,
  enum_diff: &EnumDiff,
  entries: &mut Vec<DocEntryCtx>,
) {
  for removed_member in &enum_diff.removed_members {
    let id = IdBuilder::new(render_ctx)
      .kind(IdKind::Enum)
      .name(enum_name)
      .name(&removed_member.name)
      .build();

    entries.push(DocEntryCtx::new_with_diff(
      render_ctx,
      id,
      Some(html_escape::encode_text(&removed_member.name).into_owned()),
      None,
      &removed_member
        .init
        .as_ref()
        .map(|init| format!(" = {}", render_type_def(render_ctx, init)))
        .unwrap_or_default(),
      Default::default(),
      None,
      &removed_member.location,
      Some(DiffStatus::Removed),
      None,
      None,
      None,
      None,
    ));
  }
}

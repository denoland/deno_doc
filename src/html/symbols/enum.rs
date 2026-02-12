use crate::diff::EnumDiff;
use crate::html::DiffStatus;
use crate::html::DocNodeWithContext;
use crate::html::render_context::RenderContext;
use crate::html::types::render_type_def;
use crate::html::util::*;

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
  members.sort_by(|a, b| a.name.cmp(&b.name));

  let mut items = members
    .into_iter()
    .map(|member| {
      let id = IdBuilder::new(render_ctx.ctx)
        .kind(IdKind::Enum)
        .name(doc_node.get_name())
        .component(&member.name)
        .build();

      let tags = Tag::from_js_doc(&member.js_doc);

      let diff_status = get_member_diff_status(enum_diff, &member.name);

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
        None,
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
    let id = IdBuilder::new(render_ctx.ctx)
      .kind(IdKind::Enum)
      .name(enum_name)
      .component(&removed_member.name)
      .build();

    entries.push(DocEntryCtx::new_with_diff(
      render_ctx,
      id,
      Some(
        html_escape::encode_text(&removed_member.name).into_owned(),
      ),
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
    ));
  }
}

use crate::html::DocNodeWithContext;
use crate::html::render_context::RenderContext;
use crate::html::util::*;
use std::collections::HashSet;

pub(crate) fn render_type_alias(
  ctx: &RenderContext,
  doc_node: &DocNodeWithContext,
  name: &str,
) -> Vec<SectionCtx> {
  let type_alias_def = doc_node.type_alias_def().unwrap();

  let current_type_params = type_alias_def
    .type_params
    .iter()
    .map(|def| def.name.as_str())
    .collect::<HashSet<&str>>();
  let ctx = &ctx.with_current_type_params(current_type_params);

  let type_alias_diff = ctx.ctx.diff.as_ref().and_then(|diff_index| {
    diff_index
      .get_def_diff(
        &doc_node.origin.specifier,
        doc_node.get_name(),
        doc_node.def.to_kind(),
      )
      .and_then(|d| d.as_type_alias())
  });

  let id = IdBuilder::new(ctx)
    .kind(IdKind::TypeAlias)
    .name(name)
    .build();

  let mut sections = vec![];

  if let Some(type_params) = crate::html::types::render_type_params(
    ctx,
    &doc_node.js_doc,
    &type_alias_def.type_params,
    &doc_node.location,
    type_alias_diff.and_then(|d| d.type_params_change.as_ref()),
  ) {
    sections.push(type_params);
  }

  sections.extend(super::render_type_def_sections(
    ctx,
    name,
    &type_alias_def.ts_type,
    type_alias_diff.and_then(|d| d.ts_type_change.as_ref()),
    id,
    "Definition",
    &doc_node.location,
  ));

  sections
}

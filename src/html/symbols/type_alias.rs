use crate::html::render_context::RenderContext;
use crate::html::types::render_type_def;
use crate::html::util::*;
use crate::html::DocNodeWithContext;
use std::collections::HashSet;

pub(crate) fn render_type_alias(
  ctx: &RenderContext,
  doc_node: &DocNodeWithContext,
) -> Vec<SectionCtx> {
  let type_alias_def = doc_node.type_alias_def.as_ref().unwrap();

  let current_type_params = type_alias_def
    .type_params
    .iter()
    .map(|def| def.name.as_str())
    .collect::<HashSet<&str>>();
  let ctx = &ctx.with_current_type_params(current_type_params);

  let id = name_to_id("typeAlias", doc_node.get_name());

  // TODO: tags, TypeParamsDoc

  let mut sections = vec![];

  if let Some(type_params) = crate::html::types::render_type_params(
    ctx,
    &type_alias_def.type_params,
    &doc_node.location,
  ) {
    sections.push(type_params);
  }

  sections.push(SectionCtx {
    title: "Definition",
    content: SectionContentCtx::DocEntry(vec![DocEntryCtx::new(
      ctx,
      &id,
      "",
      &render_type_def(ctx, &type_alias_def.ts_type),
      HashSet::new(),
      None,
      &doc_node.location,
    )]),
  });

  sections
}

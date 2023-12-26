use crate::html::render_context::RenderContext;
use crate::html::types::render_type_def;
use crate::html::util::*;

pub(crate) fn render_type_alias(
  ctx: &RenderContext,
  doc_node: &crate::DocNode,
) -> Vec<SectionCtx> {
  let type_alias_def = doc_node.type_alias_def.as_ref().unwrap();

  let current_type_params = type_alias_def
    .type_params
    .iter()
    .map(|def| def.name.clone())
    .collect::<std::collections::HashSet<String>>();
  let ctx = &ctx.with_current_type_params(current_type_params);

  let id = name_to_id("typeAlias", &doc_node.name);

  // TODO: tags, TypeParamsDoc

  let mut sections = vec![];

  if let Some(type_params) =
    crate::html::types::render_type_params(ctx, &type_alias_def.type_params)
  {
    sections.push(type_params);
  }

  sections.push(SectionCtx {
    title: "Definition",
    content: SectionContentCtx::DocEntry(vec![DocEntryCtx::new(
      ctx,
      &id,
      "",
      &format!(": {}", render_type_def(ctx, &type_alias_def.ts_type)),
      None,
    )]),
  });

  sections
}

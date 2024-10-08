use crate::html::render_context::RenderContext;
use crate::html::symbols::interface::render_call_signatures;
use crate::html::symbols::interface::render_index_signatures;
use crate::html::symbols::interface::render_methods;
use crate::html::symbols::interface::render_properties;
use crate::html::types::render_type_def;
use crate::html::util::*;
use crate::html::DocNodeWithContext;
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

  let id = name_to_id("typeAlias", name);

  let mut sections = vec![];

  if let Some(type_params) = crate::html::types::render_type_params(
    ctx,
    &doc_node.js_doc,
    &type_alias_def.type_params,
    &doc_node.location,
  ) {
    sections.push(type_params);
  }

  if let Some(ts_type_literal) = type_alias_def.ts_type.type_literal.as_ref() {
    if let Some(index_signatures) =
      render_index_signatures(ctx, &ts_type_literal.index_signatures)
    {
      sections.push(index_signatures);
    }

    if let Some(call_signatures) =
      render_call_signatures(ctx, &ts_type_literal.call_signatures)
    {
      sections.push(call_signatures);
    }

    if let Some(properties) =
      render_properties(ctx, name, &ts_type_literal.properties)
    {
      sections.push(properties);
    }

    if let Some(methods) = render_methods(ctx, name, &ts_type_literal.methods) {
      sections.push(methods);
    }
  } else {
    sections.push(SectionCtx::new(
      ctx,
      "Definition",
      SectionContentCtx::DocEntry(vec![DocEntryCtx::new(
        ctx,
        &id,
        None,
        None,
        &render_type_def(ctx, &type_alias_def.ts_type),
        Default::default(),
        None,
        &doc_node.location,
      )]),
    ));
  }

  sections
}

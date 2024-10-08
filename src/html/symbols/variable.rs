use crate::html::render_context::RenderContext;
use crate::html::symbols::interface::render_call_signatures;
use crate::html::symbols::interface::render_index_signatures;
use crate::html::symbols::interface::render_methods;
use crate::html::symbols::interface::render_properties;
use crate::html::types::render_type_def;
use crate::html::util::*;
use crate::html::DocNodeWithContext;

pub(crate) fn render_variable(
  ctx: &RenderContext,
  doc_node: &DocNodeWithContext,
  name: &str,
) -> Vec<SectionCtx> {
  let variable_def = doc_node.variable_def().unwrap();

  let Some(ts_type) = &variable_def.ts_type else {
    return vec![];
  };

  let id = name_to_id("variable", &doc_node.get_qualified_name());

  let mut sections = vec![];

  if let Some(ts_type_literal) = &ts_type.type_literal {
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
      "Type",
      SectionContentCtx::DocEntry(vec![DocEntryCtx::new(
        ctx,
        &id,
        None,
        None,
        &render_type_def(ctx, ts_type),
        Default::default(),
        None,
        &doc_node.location,
      )]),
    ));
  }

  sections
}

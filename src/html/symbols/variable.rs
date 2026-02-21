use crate::diff::InterfaceDiff;
use crate::html::DiffStatus;
use crate::html::DocNodeWithContext;
use crate::html::render_context::RenderContext;
use crate::html::symbols::interface::render_call_signatures;
use crate::html::symbols::interface::render_index_signatures;
use crate::html::symbols::interface::render_methods;
use crate::html::symbols::interface::render_properties;
use crate::html::types::render_type_def;
use crate::html::util::*;

pub(crate) fn render_variable(
  ctx: &RenderContext,
  doc_node: &DocNodeWithContext,
  name: &str,
) -> Vec<SectionCtx> {
  let variable_def = doc_node.variable_def().unwrap();

  let Some(ts_type) = &variable_def.ts_type else {
    return vec![];
  };

  let var_diff = ctx.ctx.diff.as_ref().and_then(|diff_index| {
    diff_index
      .get_def_diff(
        &doc_node.origin.specifier,
        doc_node.get_name(),
        doc_node.def.to_kind(),
      )
      .and_then(|d| d.as_variable())
  });

  let id = IdBuilder::new(ctx)
    .kind(IdKind::Variable)
    .name(doc_node.get_qualified_name())
    .build();

  let mut sections = vec![];

  if let Some(ts_type_literal) = &ts_type.type_literal {
    let type_lit_diff = var_diff
      .and_then(|d| d.ts_type_change.as_ref())
      .and_then(|tc| tc.old.type_literal.as_ref())
      .and_then(|old_lit| {
        InterfaceDiff::diff_type_literal(old_lit, ts_type_literal)
      });

    if let Some(index_signatures) =
      render_index_signatures(ctx, &ts_type_literal.index_signatures,
                              type_lit_diff.as_ref(),
      )
    {
      sections.push(index_signatures);
    }

    if let Some(call_signatures) =
      render_call_signatures(ctx, &ts_type_literal.call_signatures,
                             type_lit_diff.as_ref(),
      )
    {
      sections.push(call_signatures);
    }

    if let Some(properties) = render_properties(
      ctx,
      name,
      &ts_type_literal.properties,
      type_lit_diff.as_ref(),
    ) {
      sections.push(properties);
    }

    if let Some(methods) = render_methods(
      ctx,
      name,
      &ts_type_literal.methods,
      type_lit_diff.as_ref(),
    ) {
      sections.push(methods);
    }
  } else {
    let (diff_status, old_content) = if let Some(ts_type_change) =
      var_diff.and_then(|d| d.ts_type_change.as_ref())
    {
      (
        Some(DiffStatus::Modified),
        Some(render_type_def(ctx, &ts_type_change.old)),
      )
    } else {
      (None, None)
    };

    sections.push(SectionCtx::new(
      ctx,
      "Type",
      SectionContentCtx::DocEntry(vec![DocEntryCtx::new(
        ctx,
        id,
        None,
        None,
        &render_type_def(ctx, ts_type),
        Default::default(),
        None,
        &doc_node.location,
        diff_status,
        old_content,
        None,
        None,
      )]),
    ));
  }

  sections
}

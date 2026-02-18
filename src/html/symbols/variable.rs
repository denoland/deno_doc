use crate::diff::VariableDiff;
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

  // Extract VariableDiff if available
  let var_diff = ctx.ctx.diff.as_ref().and_then(|diff_index| {
    let info = diff_index.get_node_diff(
      &doc_node.origin.specifier,
      doc_node.get_name(),
      doc_node.def.to_kind(),
    )?;
    let node_diff = info.diff.as_ref()?;
    if let crate::diff::DocNodeDefDiff::Variable(var_diff) =
      node_diff.def_changes.as_ref()?
    {
      Some(var_diff)
    } else {
      None
    }
  });

  let id = IdBuilder::new(ctx)
    .kind(IdKind::Variable)
    .name(doc_node.get_qualified_name())
    .build();

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
    let (diff_status, old_content) = get_type_diff_info(ctx, var_diff);

    sections.push(SectionCtx::new(
      ctx,
      "Type",
      SectionContentCtx::DocEntry(vec![DocEntryCtx::new_with_diff(
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

fn get_type_diff_info(
  ctx: &RenderContext,
  var_diff: Option<&VariableDiff>,
) -> (Option<DiffStatus>, Option<String>) {
  let diff = match var_diff {
    Some(d) => d,
    None => return (None, None),
  };

  if let Some(ts_type_change) = &diff.ts_type_change {
    (
      Some(DiffStatus::Modified),
      Some(render_type_def(ctx, &ts_type_change.old)),
    )
  } else {
    (None, None)
  }
}

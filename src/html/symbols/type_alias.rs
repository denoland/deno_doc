use crate::diff::TypeAliasDiff;
use crate::html::DiffStatus;
use crate::html::DocNodeWithContext;
use crate::html::render_context::RenderContext;
use crate::html::symbols::interface::render_call_signatures;
use crate::html::symbols::interface::render_index_signatures;
use crate::html::symbols::interface::render_methods;
use crate::html::symbols::interface::render_properties;
use crate::html::types::render_type_def;
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

  let ta_diff = ctx.ctx.diff.as_ref().and_then(|diff_index| {
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
    ta_diff.and_then(|d| d.type_params_change.as_ref()),
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
    let (diff_status, old_content) = get_type_diff_info(ctx, ta_diff);

    sections.push(SectionCtx::new(
      ctx,
      "Definition",
      SectionContentCtx::DocEntry(vec![DocEntryCtx::new(
        ctx,
        id,
        None,
        None,
        &render_type_def(ctx, &type_alias_def.ts_type),
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
  ta_diff: Option<&TypeAliasDiff>,
) -> (Option<DiffStatus>, Option<String>) {
  let diff = match ta_diff {
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

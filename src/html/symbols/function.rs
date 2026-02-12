use super::SymbolContentCtx;
use crate::diff::FunctionDiff;
use crate::function::FunctionDef;
use crate::html::DiffStatus;
use crate::html::DocNodeWithContext;
use crate::html::parameters::render_params;
use crate::html::render_context::RenderContext;
use crate::html::types::render_type_def;
use crate::html::types::render_type_def_colon;
use crate::html::types::type_params_summary;
use crate::html::util::*;
use crate::js_doc::JsDocTag;
use crate::params::ParamPatternDef;
use indexmap::IndexSet;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashSet;
use std::ops::Deref;

#[derive(Debug, Serialize, Deserialize, Clone)]
struct OverloadRenderCtx {
  id: Id,
  anchor: AnchorCtx,
  name: String,
  summary: String,
  deprecated: Option<String>,
  content: SymbolContentCtx,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct FunctionCtx {
  functions: Vec<OverloadRenderCtx>,
}

impl FunctionCtx {
  pub const TEMPLATE: &'static str = "function";

  pub(crate) fn new(
    ctx: &RenderContext,
    doc_nodes: Vec<&DocNodeWithContext>,
  ) -> Self {
    let mut functions_content = Vec::with_capacity(doc_nodes.len());

    let overloads_count = doc_nodes
      .iter()
      .enumerate()
      .filter(|(i, doc_node)| {
        let function_def = doc_node.function_def().unwrap();

        !(function_def.has_body && *i != 0)
      })
      .count();

    for (i, doc_node) in doc_nodes.into_iter().enumerate() {
      let function_def = doc_node.function_def().unwrap();

      if function_def.has_body && i != 0 {
        continue;
      }

      let deprecated = doc_node.js_doc.tags.iter().find_map(|tag| {
        if let JsDocTag::Deprecated { doc } = tag {
          Some(
            doc
              .as_ref()
              .map(|doc| crate::html::jsdoc::render_markdown(ctx, doc, true))
              .unwrap_or_default(),
          )
        } else {
          None
        }
      });

      let overload_id = IdBuilder::new(ctx.ctx)
        .kind(IdKind::Function)
        .name(doc_node.get_name())
        .index(i)
        .build();

      if overloads_count > 1 {
        ctx.toc.add_entry(
          0,
          &format!("Overload {}", i + 1),
          overload_id.as_str(),
        );
      }

      functions_content.push(OverloadRenderCtx {
        id: overload_id.clone(),
        anchor: AnchorCtx {
          id: overload_id.clone(),
        },
        name: doc_node.get_name().to_string(),
        summary: render_function_summary(function_def, ctx),
        deprecated,
        content: render_single_function(ctx, doc_node, overload_id.clone()),
      });
    }

    FunctionCtx {
      functions: functions_content,
    }
  }
}

pub(crate) fn render_function_summary(
  function_def: &FunctionDef,
  render_ctx: &RenderContext,
) -> String {
  let return_type = function_def
    .return_type
    .as_ref()
    .map(|ts_type| render_type_def_colon(render_ctx, ts_type))
    .unwrap_or_default();

  format!(
    "{}({}){return_type}",
    type_params_summary(render_ctx, &function_def.type_params),
    render_params(render_ctx, &function_def.params)
  )
}

fn render_single_function(
  ctx: &RenderContext,
  doc_node: &DocNodeWithContext,
  overload_id: Id,
) -> SymbolContentCtx {
  let function_def = doc_node.function_def().unwrap();

  let current_type_params = function_def
    .type_params
    .iter()
    .map(|def| def.name.as_str())
    .collect::<HashSet<&str>>();
  let ctx = &ctx.with_current_type_params(current_type_params);

  // Extract FunctionDiff if available
  let func_diff = ctx.ctx.diff.as_ref().and_then(|diff_index| {
    let info = diff_index.get_node_diff(
      &doc_node.origin.specifier,
      doc_node.get_name(),
      doc_node.def.to_kind(),
    )?;
    let node_diff = info.diff.as_ref()?;
    if let crate::diff::DocNodeDefDiff::Function(func_diff) =
      node_diff.def_changes.as_ref()?
    {
      Some(func_diff)
    } else {
      None
    }
  });

  let param_docs = doc_node
    .js_doc
    .tags
    .iter()
    .filter_map(|tag| {
      if let JsDocTag::Param {
        name,
        doc,
        optional,
        default,
        ..
      } = tag
      {
        Some((name.deref(), (doc, *optional, default)))
      } else {
        None
      }
    })
    .collect::<std::collections::HashMap<
      &str,
      (&Option<Box<str>>, bool, &Option<Box<str>>),
    >>();

  let mut params = function_def
    .params
    .iter()
    .enumerate()
    .map(|(i, param)| {
      let (name, str_name) = crate::html::parameters::param_name(param, i);
      let id = IdBuilder::new(ctx.ctx)
        .component(&overload_id)
        .kind(IdKind::Parameter)
        .name(&str_name)
        .build();

      let (mut default, optional) = if let Some((_doc, optional, default)) =
        param_docs.get(name.as_str())
      {
        ((**default).to_owned(), *optional)
      } else {
        (None, false)
      };

      let ts_type =
        if let ParamPatternDef::Assign { left, right } = &param.pattern {
          default = default.or(Some(right.deref().into()));
          left.ts_type.as_ref()
        } else {
          param.ts_type.as_ref()
        };

      let mut ts_type = ts_type
        .map(|ts_type| render_type_def_colon(ctx, ts_type))
        .unwrap_or_default();

      if let Some(default) = &default
        && default.deref() != "[UNSUPPORTED]" {
          ts_type = format!(r#"{ts_type}<span><span class="font-normal"> = </span>{default}</span>"#);
        }

      let tags = if matches!(
        param.pattern,
        ParamPatternDef::Array { optional, .. }
          | ParamPatternDef::Identifier { optional, .. }
          | ParamPatternDef::Object { optional, .. }
        if optional
      ) || default.is_some()
        || optional
      {
        IndexSet::from([Tag::Optional])
      } else {
        IndexSet::new()
      };

      let param_doc = param_docs
        .get(name.as_str())
        .and_then(|(doc, _, _)| doc.as_deref());

      let (diff_status, old_content) =
        get_param_diff_info(func_diff, i);

      DocEntryCtx::new_with_diff(
        ctx,
        id,
        Some(name),
        None,
        &ts_type,
        tags,
        param_doc,
        &doc_node.location,
        diff_status,
        old_content,
        None,
      )
    })
    .collect::<Vec<DocEntryCtx>>();

  // Inject removed parameters
  if let Some(diff) = func_diff {
    inject_removed_params(ctx, diff, doc_node, &overload_id, &mut params);
  }

  let mut sections = vec![];

  let docs =
    crate::html::jsdoc::jsdoc_body_to_html(ctx, &doc_node.js_doc, false);
  let examples = crate::html::jsdoc::jsdoc_examples(ctx, &doc_node.js_doc);

  if let Some(examples) = examples {
    sections.push(examples);
  }

  if let Some(type_params) = crate::html::types::render_type_params(
    ctx,
    &doc_node.js_doc,
    &function_def.type_params,
    &doc_node.location,
    func_diff.and_then(|d| d.type_params_change.as_ref()),
  ) {
    sections.push(type_params);
  }

  if !params.is_empty() {
    sections.push(SectionCtx::new(
      ctx,
      "Parameters",
      SectionContentCtx::DocEntry(params),
    ));
  }

  sections.push(SectionCtx::new(
    ctx,
    "Return Type",
    SectionContentCtx::DocEntry(
      render_function_return_type(
        ctx,
        function_def,
        doc_node,
        overload_id.clone(),
        func_diff,
      )
      .map_or_else(Default::default, |doc_entry| vec![doc_entry]),
    ),
  ));

  let throws = doc_node
    .js_doc
    .tags
    .iter()
    .filter_map(|tag| {
      if let JsDocTag::Throws { type_ref, doc } = tag
        && (type_ref.is_some() || doc.is_some())
      {
        return Some((type_ref, doc));
      }

      None
    })
    .enumerate()
    .map(|(i, (type_ref, doc))| {
      render_function_throws(
        ctx,
        doc_node,
        type_ref,
        doc,
        overload_id.clone(),
        i,
      )
    })
    .collect::<Vec<_>>();

  if !throws.is_empty() {
    sections.push(SectionCtx::new(
      ctx,
      "Throws",
      SectionContentCtx::DocEntry(throws),
    ));
  }

  let references = doc_node
    .js_doc
    .tags
    .iter()
    .filter_map(|tag| {
      if let JsDocTag::See { doc } = tag {
        Some(super::generate_see(ctx, doc))
      } else {
        None
      }
    })
    .collect::<Vec<_>>();

  if !references.is_empty() {
    sections.push(SectionCtx::new(
      ctx,
      "See",
      SectionContentCtx::See(references),
    ));
  }

  SymbolContentCtx {
    id: Id::empty(),
    sections,
    docs,
  }
}

fn render_function_return_type(
  render_ctx: &RenderContext,
  def: &FunctionDef,
  doc_node: &DocNodeWithContext,
  overload_id: Id,
  func_diff: Option<&FunctionDiff>,
) -> Option<DocEntryCtx> {
  let return_type = def.return_type.as_ref()?;

  let id = IdBuilder::new(render_ctx.ctx)
    .component(overload_id.as_str())
    .kind(IdKind::Return)
    .build();

  let return_type_doc = doc_node.js_doc.tags.iter().find_map(|tag| {
    if let JsDocTag::Return { doc, .. } = tag {
      doc.as_deref()
    } else {
      None
    }
  });

  let (diff_status, old_content) = if let Some(diff) = func_diff {
    if let Some(return_type_change) = &diff.return_type_change {
      (
        Some(DiffStatus::Modified),
        Some(return_type_change.old.repr.to_string()),
      )
    } else {
      (None, None)
    }
  } else {
    (None, None)
  };

  Some(DocEntryCtx::new_with_diff(
    render_ctx,
    id,
    None,
    None,
    &render_type_def(render_ctx, return_type),
    IndexSet::new(),
    return_type_doc,
    &doc_node.location,
    diff_status,
    old_content,
    None,
  ))
}

fn get_param_diff_info(
  func_diff: Option<&FunctionDiff>,
  index: usize,
) -> (Option<DiffStatus>, Option<String>) {
  let diff = match func_diff {
    Some(d) => d,
    None => return (None, None),
  };

  let params_change = match &diff.params_change {
    Some(pc) => pc,
    None => return (None, None),
  };

  // Check if this param was modified
  if let Some(param_diff) = params_change.modified.iter().find(|p| p.index == index) {
    let old_content = param_diff
      .type_change
      .as_ref()
      .map(|tc| format!(": {}", &tc.old.repr));
    return (Some(DiffStatus::Modified), old_content);
  }

  // Params are positional, so added params are at indices >= old length.
  // If there's a params_change and this index is beyond the original param count,
  // it's effectively an added param. But added params are tracked separately
  // in params_change.added which doesn't have indices — they represent new
  // params appended beyond the old param list.
  // Since we're iterating over new params by index, we check if this index
  // maps to an added param.
  let old_param_count = index + 1 - params_change.added.iter().filter(|_| true).count();
  let _ = old_param_count; // not directly useful since added params don't have indices

  (None, None)
}

fn inject_removed_params(
  ctx: &RenderContext,
  func_diff: &FunctionDiff,
  doc_node: &DocNodeWithContext,
  overload_id: &Id,
  entries: &mut Vec<DocEntryCtx>,
) {
  let params_change = match &func_diff.params_change {
    Some(pc) => pc,
    None => return,
  };

  for removed_param in &params_change.removed {
    let (name, str_name) = crate::html::parameters::param_name(removed_param, entries.len());
    let id = IdBuilder::new(ctx.ctx)
      .component(overload_id)
      .kind(IdKind::Parameter)
      .name(&str_name)
      .build();

    let ts_type = removed_param
      .ts_type
      .as_ref()
      .map(|ts_type| render_type_def_colon(ctx, ts_type))
      .unwrap_or_default();

    entries.push(DocEntryCtx::new_with_diff(
      ctx,
      id,
      Some(name),
      None,
      &ts_type,
      Default::default(),
      None,
      &doc_node.location,
      Some(DiffStatus::Removed),
      None,
      None,
    ));
  }
}

fn render_function_throws(
  render_ctx: &RenderContext,
  doc_node: &DocNodeWithContext,
  type_ref: &Option<Box<str>>,
  doc: &Option<Box<str>>,
  overload_id: Id,
  throws_id: usize,
) -> DocEntryCtx {
  let id = IdBuilder::new(render_ctx.ctx)
    .component(overload_id.as_str())
    .kind(IdKind::Throws)
    .index(throws_id)
    .build();

  DocEntryCtx::new(
    render_ctx,
    id,
    None,
    None,
    type_ref
      .as_ref()
      .map(|doc| doc.as_ref())
      .unwrap_or_default(),
    IndexSet::new(),
    doc.as_ref().map(|doc| doc.as_ref()),
    &doc_node.location,
  )
}

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

      let overload_id = IdBuilder::new(ctx)
        .kind(IdKind::Function)
        .name(doc_node.get_name())
        .index(i)
        .build();

      if overloads_count > 1 {
        ctx
          .toc
          .add_entry(0, &format!("Overload {}", i + 1), &overload_id);
      }

      functions_content.push(OverloadRenderCtx {
        anchor: AnchorCtx::new(overload_id.clone()),
        name: doc_node.get_name().to_string(),
        summary: render_function_summary(
          ctx,
          &function_def.type_params,
          &function_def.params,
          &function_def.return_type,
        ),
        deprecated,
        content: render_single_function(ctx, doc_node, overload_id),
      });
    }

    FunctionCtx {
      functions: functions_content,
    }
  }
}

pub(crate) fn render_function_summary(
  render_ctx: &RenderContext,
  type_params: &[crate::ts_type_param::TsTypeParamDef],
  params: &[crate::params::ParamDef],
  return_type: &Option<crate::ts_type::TsTypeDef>,
) -> String {
  let return_type = return_type
    .as_ref()
    .map(|ts_type| render_type_def_colon(render_ctx, ts_type))
    .unwrap_or_default();

  format!(
    "{}({}){return_type}",
    type_params_summary(render_ctx, type_params),
    render_params(render_ctx, params)
  )
}

pub(crate) fn render_old_function_summary(
  ctx: &RenderContext,
  type_params: &[crate::ts_type_param::TsTypeParamDef],
  params: &[crate::params::ParamDef],
  return_type: &Option<crate::ts_type::TsTypeDef>,
  type_params_change: Option<&crate::diff::TypeParamsDiff>,
  params_change: Option<&crate::diff::ParamsDiff>,
  return_type_change: Option<&crate::diff::TsTypeDiff>,
) -> Option<String> {
  if type_params_change.is_none()
    && params_change.is_none()
    && return_type_change.is_none()
  {
    return None;
  }

  let old_type_params = match type_params_change {
    Some(type_params_change) => {
      let mut old_type_params = Vec::new();

      for tp in type_params {
        if type_params_change.added.iter().any(|a| a.name == tp.name) {
          continue;
        }

        let modified = type_params_change
          .modified
          .iter()
          .find(|m| m.name == tp.name);

        if let Some(tp_diff) = modified {
          let mut param = tp.clone();
          if let Some(cc) = &tp_diff.constraint_change {
            param.constraint = cc.old.clone();
          }
          if let Some(dc) = &tp_diff.default_change {
            param.default = dc.old.clone();
          }
          old_type_params.push(param);
        } else {
          old_type_params.push(tp.clone());
        }
      }

      old_type_params.extend(type_params_change.removed.iter().cloned());

      old_type_params
    },
    None => type_params.to_vec(),
  };

  let old_params = match params_change {
    Some(pc) => reconstruct_old_params(params, pc),
    None => params.to_vec(),
  };

  let old_return_type = match return_type_change {
    Some(rt) => Some(rt.old.clone()),
    None => return_type.clone(),
  };

  Some(render_function_summary(
    ctx,
    &old_type_params,
    &old_params,
    &old_return_type,
  ))
}

pub(crate) fn render_old_index_sig_params(
  ctx: &RenderContext,
  params: &[crate::params::ParamDef],
  params_change: &crate::diff::ParamsDiff,
) -> String {
  render_params(ctx, &reconstruct_old_params(params, params_change))
}

/// Reconstruct old params from current params + ParamsDiff.
fn reconstruct_old_params(
  params: &[crate::params::ParamDef],
  params_change: &crate::diff::ParamsDiff,
) -> Vec<crate::params::ParamDef> {
  let mut old_params = Vec::new();

  for (i, new_param) in params.iter().enumerate() {
    if params_change.added.iter().any(|p| p == new_param) {
      continue;
    }

    let modified = params_change.modified.iter().find(|p| p.index == i);

    if let Some(param_diff) = modified {
      let mut param = if let Some(pc) = &param_diff.pattern_change {
        pc.old.clone()
      } else {
        new_param.clone()
      };
      if let Some(tc) = &param_diff.type_change {
        param.ts_type = Some(tc.old.clone());
      }
      old_params.push(param);
    } else {
      old_params.push(new_param.clone());
    }
  }

  old_params.extend(params_change.removed.iter().cloned());

  old_params
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

  let direct_func_diff = ctx.ctx.diff.as_ref().and_then(|diff_index| {
    diff_index
      .get_def_diff(
        &doc_node.origin.specifier,
        doc_node.get_name(),
        doc_node.def.to_kind(),
      )
      .and_then(|d| d.as_function())
  });

  // For drilldown symbols (class/interface methods), look up via parent
  let drilldown_func_diff = if direct_func_diff.is_none() {
    get_drilldown_function_diff(ctx, doc_node)
  } else {
    None
  };

  let func_diff = direct_func_diff.or(drilldown_func_diff.as_ref());

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
      let id = IdBuilder::new_with_parent(ctx, &overload_id)
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
        get_param_diff_info(ctx, func_diff, i, param);

      DocEntryCtx::new(
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
        None,
      )
    })
    .collect::<Vec<DocEntryCtx>>();

  // Inject removed parameters
  if let Some(diff) = func_diff {
    inject_removed_params(ctx, diff, doc_node, &overload_id, &mut params);
  }

  let mut sections = vec![];

  let mut docs =
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

  let throws_tags_diff = get_js_doc_diff(ctx, doc_node)
    .and_then(|jd| jd.tags_change);

  let mut throws = doc_node
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
        &throws_tags_diff,
      )
    })
    .collect::<Vec<_>>();

  // Inject removed throws entries
  if let Some(td) = &throws_tags_diff {
    for removed_tag in &td.removed {
      if let JsDocTag::Throws { type_ref, doc } = removed_tag && (type_ref.is_some() || doc.is_some()) {
        let id = IdBuilder::new_with_parent(ctx, &overload_id)
          .kind(IdKind::Throws)
          .index(throws.len())
          .build();

        throws.push(DocEntryCtx::removed(
          ctx,
          id,
          None,
          None,
          type_ref
            .as_ref()
            .map(|t| t.as_ref())
            .unwrap_or_default(),
          IndexSet::new(),
          doc.as_ref().map(|d| d.as_ref()),
          &doc_node.location,
        ));
      }
    }
  }

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

  if ctx.ctx.diff_only
    && !crate::html::diff::is_symbol_added(doc_node)
    && !crate::html::diff::is_symbol_removed(doc_node)
  {
    crate::html::diff::filter_sections_diff_only(&mut sections, &ctx.toc);
  }

  // If there's a doc text change, re-render docs with inline diff
  if let Some(diff_docs) = render_overload_docs_with_diff(ctx, doc_node) {
    docs = Some(diff_docs);
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

  let id = IdBuilder::new_with_parent(render_ctx, &overload_id)
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
        Some(render_type_def(render_ctx, &return_type_change.old)),
      )
    } else {
      (None, None)
    }
  } else {
    (None, None)
  };

  Some(DocEntryCtx::new(
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
    None,
  ))
}

fn get_param_diff_info(
  ctx: &RenderContext,
  func_diff: Option<&FunctionDiff>,
  index: usize,
  param: &crate::params::ParamDef,
) -> (Option<DiffStatus>, Option<String>) {
  let diff = match func_diff {
    Some(d) => d,
    None => return (None, None),
  };

  let params_change = match &diff.params_change {
    Some(pc) => pc,
    None => return (None, None),
  };

  // Check if this param was added (match by value, not position)
  if params_change.added.iter().any(|p| p == param) {
    return (Some(DiffStatus::Added), None);
  }

  // Check if this param was modified
  if let Some(param_diff) =
    params_change.modified.iter().find(|p| p.index == index)
  {
    let old_name = param_diff.pattern_change.as_ref().map(|pc| {
      let (name, _) = crate::html::parameters::param_name(&pc.old, index);
      name
    });
    let old_content = param_diff
      .type_change
      .as_ref()
      .map(|tc| render_type_def_colon(ctx, &tc.old));

    let status = if let Some(old_name) = old_name {
      DiffStatus::Renamed { old_name }
    } else {
      DiffStatus::Modified
    };

    return (Some(status), old_content);
  }

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
    let (name, str_name) =
      crate::html::parameters::param_name(removed_param, entries.len());
    let id = IdBuilder::new_with_parent(ctx, overload_id)
      .kind(IdKind::Parameter)
      .name(&str_name)
      .build();

    let ts_type = removed_param
      .ts_type
      .as_ref()
      .map(|ts_type| render_type_def_colon(ctx, ts_type))
      .unwrap_or_default();

    entries.push(DocEntryCtx::removed(
      ctx,
      id,
      Some(name),
      None,
      &ts_type,
      Default::default(),
      None,
      &doc_node.location,
    ));
  }
}

#[allow(clippy::too_many_arguments)]
fn render_function_throws(
  render_ctx: &RenderContext,
  doc_node: &DocNodeWithContext,
  type_ref: &Option<Box<str>>,
  doc: &Option<Box<str>>,
  overload_id: Id,
  throws_id: usize,
  diff: &Option<crate::diff::TagsDiff>,
) -> DocEntryCtx {
  let (diff_status, old_content) = if let Some(td) = diff {
    if let Some(tag_diff) = td.modified.iter().find(|m| {
      matches!(&m.new, JsDocTag::Throws { type_ref: t, .. } if t == type_ref)
    }) {
      let old = if let JsDocTag::Throws {
        type_ref: old_type_ref,
        ..
      } = &tag_diff.old
      {
        old_type_ref
          .as_ref()
          .map(|t| t.to_string())
      } else {
        None
      };
      (Some(DiffStatus::Modified), old)
    } else if td.added.iter().any(|a| {
      matches!(a, JsDocTag::Throws { type_ref: t, .. } if t == type_ref)
    }) {
      (Some(DiffStatus::Added), None)
    } else {
      (None, None)
    }
  } else {
    (None, None)
  };


  let id = IdBuilder::new_with_parent(render_ctx, &overload_id)
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
    diff_status,
    old_content,
    None,
    None,
  )
}

/// For drilldown symbols (class/interface methods), extract the FunctionDiff
/// from the parent node's diff data.
fn get_drilldown_function_diff(
  ctx: &RenderContext,
  doc_node: &DocNodeWithContext,
) -> Option<FunctionDiff> {
  let diff_index = ctx.ctx.diff.as_ref()?;
  let drilldown_name = doc_node.drilldown_name.as_deref()?;
  let parent = doc_node.parent.as_ref()?;

  let def_changes = diff_index.get_def_diff(
    &parent.origin.specifier,
    parent.get_name(),
    parent.def.to_kind(),
  )?;

  match def_changes {
    crate::diff::DocNodeDefDiff::Class(class_diff) => class_diff
      .method_changes
      .as_ref()?
      .modified
      .iter()
      .find(|m| &*m.name == drilldown_name)?
      .function_diff
      .clone(),
    crate::diff::DocNodeDefDiff::Interface(iface_diff) => {
      let imd = iface_diff
        .method_changes
        .as_ref()?
        .modified
        .iter()
        .find(|m| m.name == drilldown_name)?;
      Some(FunctionDiff {
        params_change: imd.params_change.clone(),
        return_type_change: imd.return_type_change.clone(),
        is_async_change: None,
        is_generator_change: None,
        type_params_change: imd.type_params_change.clone(),
        decorators_change: None,
      })
    }
    _ => None,
  }
}

/// Look up the JsDocDiff for a function/method doc node.
/// Works for both top-level functions and drilldown methods (class/interface).
fn get_js_doc_diff(
  ctx: &RenderContext,
  doc_node: &DocNodeWithContext,
) -> Option<crate::diff::JsDocDiff> {
  let diff_index = ctx.ctx.diff.as_ref()?;

  if doc_node.drilldown_name.is_none() {
    // Top-level function: check node-level js_doc_changes
    let info = diff_index.get_node_diff(
      &doc_node.origin.specifier,
      doc_node.get_name(),
      doc_node.def.to_kind(),
    )?;
    info.diff.as_ref()?.js_doc_changes.clone()
  } else {
    // Drilldown method: check parent's method diff for js_doc_change
    let drilldown_name = doc_node.drilldown_name.as_deref()?;
    let parent = doc_node.parent.as_ref()?;

    let def_changes = diff_index.get_def_diff(
      &parent.origin.specifier,
      parent.get_name(),
      parent.def.to_kind(),
    )?;

    match def_changes {
      crate::diff::DocNodeDefDiff::Class(class_diff) => class_diff
        .method_changes
        .as_ref()?
        .modified
        .iter()
        .find(|m| &*m.name == drilldown_name)?
        .js_doc_change
        .clone(),
      crate::diff::DocNodeDefDiff::Interface(iface_diff) => iface_diff
        .method_changes
        .as_ref()?
        .modified
        .iter()
        .find(|m| m.name == drilldown_name)?
        .js_doc_change
        .clone(),
      _ => None,
    }
  }
}

/// If the function/method has a doc text change, render docs with inline
/// diff annotations. Returns `None` if there's no doc change.
fn render_overload_docs_with_diff(
  ctx: &RenderContext,
  doc_node: &DocNodeWithContext,
) -> Option<String> {
  let js_doc_diff = get_js_doc_diff(ctx, doc_node)?;

  let doc_change = js_doc_diff.doc_change.as_ref()?;
  let old_doc = doc_change.old.as_deref().unwrap_or_default();
  let new_doc = doc_node.js_doc.doc.as_deref().unwrap_or_default();
  crate::html::jsdoc::render_docs_with_diff(ctx, old_doc, new_doc)
}

use super::SymbolContentCtx;
use crate::Declaration;
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
use crate::ts_type::TsTypeDef;
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
    symbol: &DocNodeWithContext,
    decls: Vec<&Declaration>,
  ) -> Self {
    let mut functions_content = Vec::with_capacity(decls.len());

    let overloads_count = decls
      .iter()
      .enumerate()
      .filter(|(i, decl)| {
        let function_def = decl.function_def().unwrap();

        !(function_def.has_body && *i != 0)
      })
      .count();

    for (i, decl) in decls.into_iter().enumerate() {
      let function_def = decl.function_def().unwrap();

      if function_def.has_body && i != 0 {
        continue;
      }

      let deprecated = decl.js_doc.tags.iter().find_map(|tag| {
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
        .name(symbol.get_name())
        .index(i)
        .build();

      if overloads_count > 1 {
        ctx
          .toc
          .add_entry(0, &format!("Overload {}", i + 1), &overload_id);
      }

      functions_content.push(OverloadRenderCtx {
        anchor: AnchorCtx::new(overload_id.clone()),
        name: symbol.get_name().to_string(),
        summary: render_function_summary(
          ctx,
          &function_def.type_params,
          &function_def.params,
          &function_def.return_type,
        ),
        deprecated,
        content: render_single_function(ctx, symbol, decl, overload_id),
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
    }
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
  symbol: &DocNodeWithContext,
  decl: &Declaration,
  overload_id: Id,
) -> SymbolContentCtx {
  let function_def = decl.function_def().unwrap();

  let current_type_params = function_def
    .type_params
    .iter()
    .map(|def| def.name.as_str())
    .collect::<HashSet<&str>>();
  let ctx = &ctx.with_current_type_params(current_type_params);

  let direct_func_diff = ctx.ctx.diff.as_ref().and_then(|diff_index| {
    diff_index
      .get_def_diff(
        &symbol.origin.specifier,
        symbol.get_name(),
        decl.def.to_kind(),
      )
      .and_then(|d| d.as_function())
  });

  // For drilldown symbols (class/interface methods), look up via parent
  let drilldown_func_diff = if direct_func_diff.is_none() {
    get_drilldown_function_diff(ctx, symbol)
  } else {
    None
  };

  let func_diff = direct_func_diff.or(drilldown_func_diff.as_ref());

  let param_docs = decl
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
        &decl.location,
        diff_status,
        old_content,
        None,
        None,
      )
    })
    .collect::<Vec<DocEntryCtx>>();

  // Inject removed parameters
  if let Some(diff) = func_diff {
    inject_removed_params(ctx, diff, decl, &overload_id, &mut params);
  }

  let mut sections = vec![];

  let mut docs =
    crate::html::jsdoc::jsdoc_body_to_html(ctx, &decl.js_doc, false);
  if let Some(examples) = crate::html::jsdoc::jsdoc_examples(ctx, &decl.js_doc)
  {
    sections.push(examples);
  }

  if let Some(type_params) = crate::html::types::render_type_params(
    ctx,
    &decl.js_doc,
    &function_def.type_params,
    &decl.location,
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
        decl,
        overload_id.clone(),
        func_diff,
      )
      .map_or_else(Default::default, |doc_entry| vec![doc_entry]),
    ),
  ));

  let throws_tags_diff =
    get_js_doc_diff(ctx, symbol, decl).and_then(|jd| jd.tags_change);

  let mut throws = decl
    .js_doc
    .tags
    .iter()
    .filter_map(|tag| {
      if let JsDocTag::Throws { ts_type, doc, .. } = tag
        && (ts_type.is_some() || doc.is_some())
      {
        return Some((ts_type, doc));
      }

      None
    })
    .enumerate()
    .map(|(i, (ts_type, doc))| {
      render_function_throws(
        ctx,
        decl,
        ts_type,
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
      if let JsDocTag::Throws { ts_type, doc, .. } = removed_tag
        && (ts_type.is_some() || doc.is_some())
      {
        let id = IdBuilder::new_with_parent(ctx, &overload_id)
          .kind(IdKind::Throws)
          .index(throws.len())
          .build();

        throws.push(DocEntryCtx::removed(
          ctx,
          id,
          None,
          None,
          ts_type
            .as_ref()
            .map(|t| t.repr.as_str())
            .unwrap_or_default(),
          IndexSet::new(),
          doc.as_ref().map(|d| d.as_ref()),
          &decl.location,
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

  let references = decl
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

  let decl_kind = decl.def.to_kind();
  if ctx.ctx.diff_only
    && !crate::html::diff::is_decl_added(symbol, decl_kind, &ctx.ctx.diff)
    && !crate::html::diff::is_decl_removed(symbol, decl_kind, &ctx.ctx.diff)
  {
    crate::html::diff::filter_sections_diff_only(&mut sections, &ctx.toc);
  }

  // If there's a doc text change, re-render docs with inline diff;
  // otherwise in diff_only mode, hide the unchanged docs.
  if let Some(diff_docs) = render_overload_docs_with_diff(ctx, symbol, decl) {
    docs = Some(diff_docs);
  } else if ctx.ctx.diff_only
    && !crate::html::diff::is_decl_added(symbol, decl_kind, &ctx.ctx.diff)
    && !crate::html::diff::is_decl_removed(symbol, decl_kind, &ctx.ctx.diff)
  {
    docs = None;
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
  decl: &Declaration,
  overload_id: Id,
  func_diff: Option<&FunctionDiff>,
) -> Option<DocEntryCtx> {
  let return_type = def.return_type.as_ref()?;

  let id = IdBuilder::new_with_parent(render_ctx, &overload_id)
    .kind(IdKind::Return)
    .build();

  let return_type_doc = decl.js_doc.tags.iter().find_map(|tag| {
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
    &decl.location,
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
  decl: &Declaration,
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
      &decl.location,
    ));
  }
}

#[allow(clippy::too_many_arguments)]
fn render_function_throws(
  render_ctx: &RenderContext,
  decl: &Declaration,
  ts_type: &Option<TsTypeDef>,
  doc: &Option<Box<str>>,
  overload_id: Id,
  throws_id: usize,
  diff: &Option<crate::diff::TagsDiff>,
) -> DocEntryCtx {
  let (diff_status, old_content) =
    if let Some(td) = diff {
      if let Some(tag_diff) = td.modified.iter().find(|m| {
      matches!(&m.new, JsDocTag::Throws { ts_type: t, .. } if t == ts_type)
    }) {
      let old = if let JsDocTag::Throws {
        ts_type: old_ts_type,
        ..
      } = &tag_diff.old
      {
        old_ts_type.as_ref().map(|t| t.repr.clone())
      } else {
        None
      };
      (Some(DiffStatus::Modified), old)
    } else if td.added.iter().any(|a| {
      matches!(a, JsDocTag::Throws { ts_type: t, .. } if t == ts_type)
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
    ts_type
      .as_ref()
      .map(|t| t.repr.as_str())
      .unwrap_or_default(),
    IndexSet::new(),
    doc.as_ref().map(|doc| doc.as_ref()),
    &decl.location,
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
  symbol: &DocNodeWithContext,
) -> Option<FunctionDiff> {
  let diff_index = ctx.ctx.diff.as_ref()?;
  let drilldown_name = symbol.drilldown_name.as_deref()?;
  let parent = symbol.parent.as_ref()?;

  let def_changes = parent.declarations.iter().find_map(|decl| {
    diff_index.get_def_diff(
      &parent.origin.specifier,
      parent.get_name(),
      decl.def.to_kind(),
    )
  })?;

  match def_changes {
    crate::diff::DeclarationDefDiff::Class(class_diff) => class_diff
      .method_changes
      .as_ref()?
      .modified
      .iter()
      .find(|m| &*m.name == drilldown_name)?
      .function_diff
      .clone(),
    crate::diff::DeclarationDefDiff::Interface(iface_diff) => {
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
  symbol: &DocNodeWithContext,
  decl: &Declaration,
) -> Option<crate::diff::JsDocDiff> {
  let diff_index = ctx.ctx.diff.as_ref()?;

  if symbol.drilldown_name.is_none() {
    // Top-level function: check node-level js_doc_changes
    let info = diff_index.get_declaration_diff(
      &symbol.origin.specifier,
      symbol.get_name(),
      decl.def.to_kind(),
    )?;
    info.js_doc_changes.clone()
  } else {
    // Drilldown method: check parent's method diff for js_doc_change
    let drilldown_name = symbol.drilldown_name.as_deref()?;
    let parent = symbol.parent.as_ref()?;

    let def_changes = parent.declarations.iter().find_map(|decl| {
      diff_index.get_def_diff(
        &parent.origin.specifier,
        parent.get_name(),
        decl.def.to_kind(),
      )
    })?;

    match def_changes {
      crate::diff::DeclarationDefDiff::Class(class_diff) => class_diff
        .method_changes
        .as_ref()?
        .modified
        .iter()
        .find(|m| &*m.name == drilldown_name)?
        .js_doc_change
        .clone(),
      crate::diff::DeclarationDefDiff::Interface(iface_diff) => iface_diff
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
  symbol: &DocNodeWithContext,
  decl: &Declaration,
) -> Option<String> {
  let js_doc_diff = get_js_doc_diff(ctx, symbol, decl)?;

  let doc_change = js_doc_diff.doc_change.as_ref()?;
  let old_doc = doc_change.old.as_deref().unwrap_or_default();
  let new_doc = decl.js_doc.doc.as_deref().unwrap_or_default();
  crate::html::jsdoc::render_docs_with_diff(ctx, old_doc, new_doc)
}

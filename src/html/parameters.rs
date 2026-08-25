use super::render_context::RenderContext;
use super::types::render_type_def_colon;
use super::types::with_trailing_comma;
use crate::html::DiffStatus;
use crate::html::util::DocEntryCtx;
use crate::html::util::Id;
use crate::html::util::IdBuilder;
use crate::html::util::IdKind;
use crate::html::util::Tag;
use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;
use crate::params::ParamDef;
use crate::params::ParamPatternDef;
use indexmap::IndexSet;
use std::ops::Deref;

/// Builds one [`DocEntryCtx`] per parameter, pairing each with the `@param`
/// documentation `js_doc` carries for it.
///
/// `diff_info` supplies the diff status for a parameter; callers that render
/// signatures the differ does not descend into pass `|_, _| (None, None)`.
/// The bare identifier a parameter binds, used to match it against its
/// `@param` JSDoc tag. Unwraps rest (`...rest` → `rest`) and default
/// (`x = 1` → `x`) parameters. Returns `None` for array/object destructuring
/// patterns, which bind no single name and so can't be matched by name.
fn param_doc_name(param: &ParamDef) -> Option<&str> {
  match &param.pattern {
    ParamPatternDef::Identifier { name, .. } => Some(name),
    ParamPatternDef::Rest { arg } => param_doc_name(arg),
    ParamPatternDef::Assign { left, .. } => param_doc_name(left),
    ParamPatternDef::Array { .. } | ParamPatternDef::Object { .. } => None,
  }
}

pub(crate) fn render_param_doc_entries(
  ctx: &RenderContext,
  params: &[ParamDef],
  js_doc: &JsDoc,
  parent_id: &Id,
  location: &crate::Location,
  mut diff_info: impl FnMut(
    usize,
    &ParamDef,
  ) -> (Option<DiffStatus>, Option<String>),
) -> Vec<DocEntryCtx> {
  let param_docs = js_doc
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

  // `@param` tags in source order, excluding property documentation like
  // `@param options.field`, for the positional fallback below.
  let positional_param_docs = js_doc
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
        if name.contains('.') {
          return None;
        }
        Some((name.deref(), (doc, *optional, default)))
      } else {
        None
      }
    })
    .collect::<Vec<(&str, (&Option<Box<str>>, bool, &Option<Box<str>>))>>();

  let bound_names = params
    .iter()
    .filter_map(param_doc_name)
    .collect::<std::collections::HashSet<_>>();

  params
    .iter()
    .enumerate()
    .map(|(i, param)| {
      // A destructuring pattern binds no single name, so it falls back to
      // matching its `@param` tag by position, taking the tag's name as the
      // display name — mirroring how TSDoc documents destructured
      // parameters (see issue #574). A tag naming an actual binding never
      // matches positionally, so a named parameter's documentation can't be
      // taken by a destructured one.
      let positional_doc = if matches!(
        param.pattern,
        ParamPatternDef::Array { .. } | ParamPatternDef::Object { .. }
      ) {
        positional_param_docs
          .get(i)
          .filter(|(tag_name, _)| !bound_names.contains(tag_name))
      } else {
        None
      };

      let (name, str_name) = if let Some((tag_name, _)) = positional_doc {
        (
          html_escape::encode_text(tag_name).into_owned(),
          (*tag_name).to_string(),
        )
      } else {
        param_name(param, i)
      };

      // Match the parameter to its `@param` tag by the bare identifier it
      // binds. A rest parameter renders as `...rest` but is documented as
      // `rest`, and a default (`Assign`) wraps the real binding, so unwrap
      // both.
      let param_doc = param_doc_name(param)
        .and_then(|n| param_docs.get(n))
        .or_else(|| positional_doc.map(|(_, tag_doc)| tag_doc));
      let id = IdBuilder::new_with_parent(ctx, parent_id)
        .kind(IdKind::Parameter)
        .name(&str_name)
        .build();

      let (mut default, optional) =
        if let Some((_doc, optional, default)) = param_doc {
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

      let param_doc = param_doc.and_then(|(doc, _, _)| doc.as_deref());

      let (diff_status, old_content) = diff_info(i, param);

      DocEntryCtx::new(
        ctx,
        id,
        Some(name),
        None,
        &ts_type,
        tags,
        param_doc,
        location,
        diff_status,
        old_content,
        None,
        None,
      )
    })
    .collect()
}

/// Attaches the `@param` and `@returns` documentation `js_doc` carries to an
/// entry that has no symbol page of its own to carry a Parameters section.
///
/// Class constructors, construct signatures and call signatures all render
/// inline on their parent's page and have no drilldown page, so without this
/// their parameter and return documentation is shown nowhere at all.
pub(crate) fn attach_signature_docs(
  ctx: &RenderContext,
  entry: &mut DocEntryCtx,
  params: &[ParamDef],
  js_doc: &JsDoc,
  parent_id: &Id,
  location: &crate::Location,
) {
  let has_param_docs = js_doc
    .tags
    .iter()
    .any(|tag| matches!(tag, JsDocTag::Param { doc, .. } if doc.is_some()));
  if has_param_docs {
    entry.params = render_param_doc_entries(
      ctx,
      params,
      js_doc,
      parent_id,
      location,
      |_, _| (None, None),
    );
  }

  entry.return_doc = js_doc.tags.iter().find_map(|tag| {
    if let JsDocTag::Return { doc, .. } = tag {
      doc
        .as_deref()
        .map(|doc| crate::html::jsdoc::render_markdown(ctx, doc, true))
    } else {
      None
    }
  });
}

pub(crate) fn render_params(
  ctx: &RenderContext,
  params: &[ParamDef],
) -> String {
  if params.is_empty() {
    String::new()
  } else if params.len() == 1 {
    format!("<span>{}</span>", render_param(ctx, &params[0], 0))
  } else {
    let last = params.len() - 1;
    let mut items = Vec::with_capacity(params.len());

    for (i, def) in params.iter().enumerate() {
      let rendered = render_param(ctx, def, i);
      let content = if i < last {
        with_trailing_comma(&rendered)
      } else {
        rendered
      };
      items.push(format!("<div>{content}</div>"));
    }

    let content = items.join("");

    format!(r#"<div class="ml-4">{content}</div>"#)
  }
}

fn render_param(ctx: &RenderContext, param: &ParamDef, i: usize) -> String {
  let (name, _str_name) = param_name(param, i);
  let ts_type = if let ParamPatternDef::Assign { left, .. } = &param.pattern {
    left.ts_type.as_ref().or(param.ts_type.as_ref())
  } else {
    param.ts_type.as_ref()
  };

  let ts_type = ts_type
    .map(|ts_type| render_type_def_colon(ctx, ts_type))
    .unwrap_or_default();

  let question_mark = match param.pattern {
    ParamPatternDef::Array { optional, .. } if optional => {
      r#"<span class="td-op">?</span>"#
    }
    ParamPatternDef::Assign { .. } => r#"<span class="td-op">?</span>"#,
    ParamPatternDef::Identifier { optional, .. } if optional => {
      r#"<span class="td-op">?</span>"#
    }
    ParamPatternDef::Object { optional, .. } if optional => {
      r#"<span class="td-op">?</span>"#
    }
    _ => "",
  };

  format!("<span>{name}{question_mark}{ts_type}</span>")
}

pub(crate) fn param_name(param: &ParamDef, i: usize) -> (String, String) {
  match &param.pattern {
    ParamPatternDef::Array { .. } | ParamPatternDef::Object { .. } => (
      format!(r#"<span class="italic">arg_{i}</span>"#),
      format!("arg_{i}"),
    ),
    ParamPatternDef::Assign { left, .. } => param_name(left, i),
    ParamPatternDef::Identifier { name, .. } => {
      (html_escape::encode_text(name).into_owned(), name.clone())
    }
    ParamPatternDef::Rest { arg } => (
      format!("<span>...{}</span>", param_name(arg, i).0),
      format!("...{}", param_name(arg, i).1),
    ),
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn destructured(pattern: ParamPatternDef) -> ParamDef {
    ParamDef {
      pattern,
      decorators: Box::new([]),
      ts_type: None,
    }
  }

  #[test]
  fn unnamed_object_param_is_named_by_index() {
    let param = destructured(ParamPatternDef::Object {
      props: vec![],
      optional: false,
    });
    let (html, str_name) = param_name(&param, 0);
    assert_eq!(html, r#"<span class="italic">arg_0</span>"#);
    assert_eq!(str_name, "arg_0");
  }

  #[test]
  fn unnamed_array_param_is_named_by_index() {
    let param = destructured(ParamPatternDef::Array {
      elements: vec![],
      optional: false,
    });
    let (html, str_name) = param_name(&param, 2);
    assert_eq!(html, r#"<span class="italic">arg_2</span>"#);
    assert_eq!(str_name, "arg_2");
  }
}

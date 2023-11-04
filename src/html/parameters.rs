use super::types::render_type_def;
use super::util::*;
use super::GenerateCtx;
use crate::params::ParamDef;
use crate::params::ParamPatternDef;

pub(super) fn render_params(
  ctx: &GenerateCtx,
  params: &[ParamDef],
  render_ctx: &RenderContext,
) -> String {
  if params.is_empty() {
    String::new()
  } else if params.len() <= 3 {
    let items = params
      .iter()
      .enumerate()
      .map(|(i, element)| render_param(ctx, element, i, render_ctx))
      .collect::<Vec<String>>()
      .join("<span>, </span>");

    format!("<span>{items}</span>")
  } else {
    // TODO(bartlomieju): refactor to use template
    let mut items = Vec::with_capacity(params.len());

    for (i, def) in params.iter().enumerate() {
      items.push(format!(
        "<div>{}</div>",
        render_param(ctx, def, i, render_ctx)
      ));
    }

    let content = items.join("");

    format!(r#"<div class="ident">{content}</div>"#)
  }
}

fn render_param(
  ctx: &GenerateCtx,
  param: &ParamDef,
  i: usize,
  render_ctx: &RenderContext,
) -> String {
  let (name, _str_name) = param_name(param, i);
  let ts_type = if let ParamPatternDef::Assign { left, .. } = &param.pattern {
    left.ts_type.as_ref()
  } else {
    param.ts_type.as_ref()
  };

  let ts_type = ts_type
    .map(|ts_type| {
      format!(
        r#"<span>: {}</span>"#,
        render_type_def(ctx, ts_type, render_ctx)
      )
    })
    .unwrap_or_default();

  let question_mark = match param.pattern {
    ParamPatternDef::Array { optional, .. } if optional => "?",
    ParamPatternDef::Assign { .. } => "?",
    ParamPatternDef::Identifier { optional, .. } if optional => "?",
    ParamPatternDef::Object { optional, .. } if optional => "?",
    _ => "",
  };

  format!("<span>{name}{question_mark}{ts_type}</span>")
}

pub fn param_name(param: &ParamDef, i: usize) -> (String, String) {
  match &param.pattern {
    ParamPatternDef::Array { .. } | ParamPatternDef::Object { .. } => (
      format!(r#"<span class="italic">unnamed {i}</span>"#),
      format!(r#"(unnamed {i})"#),
    ),
    ParamPatternDef::Assign { left, .. } => param_name(left, i),
    ParamPatternDef::Identifier { name, .. } => (name.clone(), name.clone()),
    ParamPatternDef::Rest { arg } => (
      format!("<span>...{}</span>", param_name(arg, i).0),
      format!("(...{})", param_name(arg, i).1),
    ),
  }
}

// TODO: classes: italic

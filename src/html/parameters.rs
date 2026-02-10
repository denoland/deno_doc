use super::render_context::RenderContext;
use super::types::render_type_def_colon;
use crate::params::ParamDef;
use crate::params::ParamPatternDef;

pub(crate) fn render_params(
  ctx: &RenderContext,
  params: &[ParamDef],
) -> String {
  if params.is_empty() {
    String::new()
  } else if params.len() == 1 {
    format!("<span>{}</span>", render_param(ctx, &params[0], 0))
  } else {
    let mut items = Vec::with_capacity(params.len());

    for (i, def) in params.iter().enumerate() {
      items.push(format!("<div>{},</div>", render_param(ctx, def, i)));
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
    ParamPatternDef::Array { optional, .. } if optional => r#"<span class="td-op">?</span>"#,
    ParamPatternDef::Assign { .. } => r#"<span class="td-op">?</span>"#,
    ParamPatternDef::Identifier { optional, .. } if optional => r#"<span class="td-op">?</span>"#,
    ParamPatternDef::Object { optional, .. } if optional => r#"<span class="td-op">?</span>"#,
    _ => "",
  };

  format!("<span>{name}{question_mark}{ts_type}</span>")
}

pub(crate) fn param_name(param: &ParamDef, i: usize) -> (String, String) {
  match &param.pattern {
    ParamPatternDef::Array { .. } | ParamPatternDef::Object { .. } => (
      format!(r#"<span class="italic">unnamed {i}</span>"#),
      format!(r#"(unnamed {i})"#),
    ),
    ParamPatternDef::Assign { left, .. } => param_name(left, i),
    ParamPatternDef::Identifier { name, .. } => {
      (html_escape::encode_text(name).into_owned(), name.clone())
    }
    ParamPatternDef::Rest { arg } => (
      format!("<span>...{}</span>", param_name(arg, i).0),
      format!("(...{})", param_name(arg, i).1),
    ),
  }
}

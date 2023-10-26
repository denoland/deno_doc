use crate::html::types::render_type_def;
use crate::params::ParamDef;
use crate::params::ParamPatternDef;

pub fn render_params(params: &[ParamDef]) -> String {
  if params.is_empty() {
    String::new()
  } else if params.len() <= 3 {
    let items = params
      .iter()
      .enumerate()
      .map(|(i, element)| render_param(element, i))
      .collect::<Vec<String>>()
      .join(&format!("<span>, </span>"));

    format!("<span>{items}</span>")
  } else {
    let items = params
      .iter()
      .enumerate()
      .map(|(i, def)| format!("<div>{}</div>", render_param(def, i)))
      .collect::<String>();

    format!(r#"<div class="ident">{items}</div>"#)
  }
}

fn render_param(param: &ParamDef, i: usize) -> String {
  let name = param_name(param, i);
  let ts_type = if let ParamPatternDef::Assign { left, .. } = &param.pattern {
    left.ts_type.as_ref()
  } else {
    param.ts_type.as_ref()
  };

  let ts_type = ts_type
    .map(|ts_type| format!(r#"<span>: {}</span>"#, render_type_def(ts_type)))
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

pub fn param_name(param: &ParamDef, i: usize) -> String {
  match &param.pattern {
    ParamPatternDef::Array { .. } | ParamPatternDef::Object { .. } => {
      format!(r#"<span class="italic">unnamed {i}</span>"#)
    }
    ParamPatternDef::Assign { left, .. } => param_name(&left, i),
    ParamPatternDef::Identifier { name, .. } => name.clone(),
    ParamPatternDef::Rest { arg } => {
      format!("<span>...{}</span>", param_name(&arg, i))
    }
  }
}

// TODO: classes: italic

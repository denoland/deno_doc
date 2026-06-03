// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::node::Location;
use crate::util::swc::get_location;

use deno_ast::oxc::ast::ast::Decorator;
use deno_ast::oxc::ast::ast::Expression;
use deno_ast::oxc::span::GetSpan;
use deno_graph::symbols::EsModuleInfo;
use deno_terminal::colors;
use serde::Deserialize;
use serde::Serialize;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct DecoratorDef {
  pub name: String,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub args: Vec<String>,
  pub location: Location,
}

impl Display for DecoratorDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "@{}", self.name)?;
    if !self.args.is_empty() {
      let args = self
        .args
        .iter()
        .map(|a| a.to_string())
        .collect::<Vec<String>>()
        .join(", ");
      write!(f, "{}{}{}", colors::cyan("("), args, colors::cyan(")"))?;
    }
    Ok(())
  }
}

impl DecoratorDef {
  fn from_ast_decorator(
    module_info: &EsModuleInfo,
    decorator: &Decorator,
  ) -> Self {
    match &decorator.expression {
      Expression::CallExpression(call_expr) => {
        if let Expression::Identifier(ident) = &call_expr.callee {
          let args = call_expr
            .arguments
            .iter()
            .map(|a| {
              let span = a.span();
              module_info.source_text()[span.start as usize..span.end as usize]
                .to_string()
            })
            .collect();
          return Self {
            name: ident.name.to_string(),
            args,
            location: get_location(module_info, ident.span.start),
          };
        }
        Self {
          name: "[UNSUPPORTED]".to_string(),
          args: vec![],
          location: get_location(module_info, call_expr.span.start),
        }
      }
      Expression::Identifier(ident) => Self {
        name: ident.name.to_string(),
        args: vec![],
        location: get_location(module_info, ident.span.start),
      },
      _ => Self {
        name: "[UNSUPPORTED]".to_string(),
        args: vec![],
        location: get_location(module_info, decorator.span.start),
      },
    }
  }
}

pub fn decorators_to_defs(
  module_info: &EsModuleInfo,
  decorators: &[Decorator],
) -> Box<[DecoratorDef]> {
  decorators
    .iter()
    .map(|d| DecoratorDef::from_ast_decorator(module_info, d))
    .collect()
}

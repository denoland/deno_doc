// Copyright 2020-2022 the Deno authors. All rights reserved. MIT license.

use crate::colors;
use crate::node::Location;
use crate::swc_util::get_location;

use deno_ast::swc::ast::Decorator;
use deno_ast::swc::ast::Expr;
use deno_ast::ParsedSource;
use deno_ast::SwcSourceRanged;
use serde::Deserialize;
use serde::Serialize;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct DecoratorDef {
  pub name: String,
  #[serde(skip_serializing_if = "Vec::is_empty")]
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
    parsed_source: &ParsedSource,
    decorator: &Decorator,
  ) -> Self {
    match decorator.expr.as_ref() {
      Expr::Call(call_expr) => {
        if let Some(expr) = call_expr.callee.clone().expr() {
          if let Expr::Ident(ident) = expr.as_ref() {
            let args = call_expr
              .args
              .iter()
              .map(|a| {
                parsed_source.text_info().range_text(&a.range()).to_string()
              })
              .collect();
            return Self {
              name: ident.sym.to_string(),
              args,
              location: get_location(parsed_source, ident.start()),
            };
          }
        }
        Self {
          name: "[UNSUPPORTED]".to_string(),
          args: vec![],
          location: get_location(parsed_source, call_expr.start()),
        }
      }
      Expr::Ident(ident) => Self {
        name: ident.sym.to_string(),
        args: vec![],
        location: get_location(parsed_source, ident.start()),
      },
      _ => Self {
        name: "[UNSUPPORTED]".to_string(),
        args: vec![],
        location: get_location(parsed_source, decorator.start()),
      },
    }
  }
}

pub fn decorators_to_defs(
  parsed_source: &ParsedSource,
  decorators: &[Decorator],
) -> Vec<DecoratorDef> {
  decorators
    .iter()
    .map(|d| DecoratorDef::from_ast_decorator(parsed_source, d))
    .collect()
}

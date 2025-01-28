// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::node::Location;
use crate::util::swc::get_location;

use deno_ast::swc::ast::Decorator;
use deno_ast::swc::ast::Expr;
use deno_ast::SourceRangedForSpanned;
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
    match decorator.expr.as_ref() {
      Expr::Call(call_expr) => {
        if let Some(expr) = call_expr.callee.clone().expr() {
          if let Expr::Ident(ident) = expr.as_ref() {
            let args = call_expr
              .args
              .iter()
              .map(|a| {
                a.text_fast(module_info.source().text_info_lazy())
                  .to_string()
              })
              .collect();
            return Self {
              name: ident.sym.to_string(),
              args,
              location: get_location(module_info, ident.start()),
            };
          }
        }
        Self {
          name: "[UNSUPPORTED]".to_string(),
          args: vec![],
          location: get_location(module_info, call_expr.start()),
        }
      }
      Expr::Ident(ident) => Self {
        name: ident.sym.to_string(),
        args: vec![],
        location: get_location(module_info, ident.start()),
      },
      _ => Self {
        name: "[UNSUPPORTED]".to_string(),
        args: vec![],
        location: get_location(module_info, decorator.start()),
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

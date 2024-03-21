// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
use crate::ts_type::TsTypeDef;
use deno_ast::swc::ast::TsTypeParam;
use deno_ast::swc::ast::TsTypeParamDecl;
use deno_ast::ParsedSource;
use serde::Deserialize;
use serde::Serialize;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct TsTypeParamDef {
  pub name: String,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub constraint: Option<TsTypeDef>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub default: Option<TsTypeDef>,
}

impl Display for TsTypeParamDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{}", self.name)?;
    if let Some(constraint) = &self.constraint {
      write!(f, " extends {}", constraint)?;
    }
    if let Some(default) = &self.default {
      write!(f, " = {}", default)?;
    }
    Ok(())
  }
}

impl TsTypeParamDef {
  pub fn new(parsed_source: &ParsedSource, param: &TsTypeParam) -> Self {
    let name = param.name.sym.to_string();
    let constraint = param
      .constraint
      .as_ref()
      .map(|constraint| TsTypeDef::new(parsed_source, constraint));
    let default = param
      .default
      .as_ref()
      .map(|default| TsTypeDef::new(parsed_source, default));

    TsTypeParamDef {
      name,
      constraint,
      default,
    }
  }
}

pub fn maybe_type_param_decl_to_type_param_defs(
  parsed_source: &ParsedSource,
  maybe_type_param_decl: Option<&TsTypeParamDecl>,
) -> Vec<TsTypeParamDef> {
  if let Some(type_params_decl) = maybe_type_param_decl {
    type_params_decl
      .params
      .iter()
      .map(|type_param| TsTypeParamDef::new(parsed_source, type_param))
      .collect::<Vec<_>>()
  } else {
    vec![]
  }
}

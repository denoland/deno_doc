// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::ts_type::TsTypeDef;
use deno_ast::swc::ast::TsTypeParam;
use deno_ast::swc::ast::TsTypeParamDecl;
use deno_graph::symbols::EsModuleInfo;
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
  pub fn new(module_info: &EsModuleInfo, param: &TsTypeParam) -> Self {
    let name = param.name.sym.to_string();
    let constraint = param
      .constraint
      .as_ref()
      .map(|constraint| TsTypeDef::new(module_info, constraint));
    let default = param
      .default
      .as_ref()
      .map(|default| TsTypeDef::new(module_info, default));

    TsTypeParamDef {
      name,
      constraint,
      default,
    }
  }
}

pub(crate) fn maybe_type_param_decl_to_type_param_defs(
  module_info: &EsModuleInfo,
  maybe_type_param_decl: Option<&TsTypeParamDecl>,
) -> Box<[TsTypeParamDef]> {
  if let Some(type_params_decl) = maybe_type_param_decl {
    type_params_decl
      .params
      .iter()
      .map(|type_param| TsTypeParamDef::new(module_info, type_param))
      .collect::<Box<[_]>>()
  } else {
    Box::new([])
  }
}

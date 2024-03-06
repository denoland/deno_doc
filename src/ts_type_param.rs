// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
use crate::ts_type::TsTypeDef;
use deno_ast::swc::ast::TsTypeParam;
use deno_ast::swc::ast::TsTypeParamDecl;
use deno_ast::ParsedSource;
use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter, Result as FmtResult};

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

impl From<(&TsTypeParam, &ParsedSource)> for TsTypeParamDef {
  fn from(
    (param, parsed_source): (&TsTypeParam, &ParsedSource),
  ) -> TsTypeParamDef {
    let name = param.name.sym.to_string();
    let constraint: Option<TsTypeDef> =
      if let Some(ts_type) = param.constraint.as_ref() {
        let type_def: TsTypeDef = (&**ts_type, parsed_source).into();
        Some(type_def)
      } else {
        None
      };
    let default: Option<TsTypeDef> =
      if let Some(ts_type) = param.default.as_ref() {
        let type_def: TsTypeDef = (&**ts_type, parsed_source).into();
        Some(type_def)
      } else {
        None
      };

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
      .map(|type_param| (type_param, parsed_source).into())
      .collect::<Vec<TsTypeParamDef>>()
  } else {
    vec![]
  }
}

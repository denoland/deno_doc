// Copyright 2020-2022 the Deno authors. All rights reserved. MIT license.

use crate::decorators::decorators_to_defs;
use crate::decorators::DecoratorDef;
use crate::params::param_to_param_def;
use crate::swc_util::is_false;
use crate::ts_type::ts_type_ann_to_def;
use crate::ts_type::TsTypeDef;
use crate::ts_type_param::maybe_type_param_decl_to_type_param_defs;
use crate::ts_type_param::TsTypeParamDef;
use crate::ParamDef;
use deno_ast::ParsedSource;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct FunctionDef {
  pub params: Vec<ParamDef>,
  pub return_type: Option<TsTypeDef>,
  #[serde(skip_serializing_if = "is_false")]
  pub has_body: bool,
  pub is_async: bool,
  pub is_generator: bool,
  pub type_params: Vec<TsTypeParamDef>,
  #[serde(skip_serializing_if = "Vec::is_empty")]
  pub decorators: Vec<DecoratorDef>,
}

pub fn function_to_function_def(
  parsed_source: &ParsedSource,
  function: &deno_ast::swc::ast::Function,
) -> FunctionDef {
  let params = function
    .params
    .iter()
    .map(|param| param_to_param_def(parsed_source, param))
    .collect();

  let maybe_return_type = function.return_type.as_deref().map(ts_type_ann_to_def);

  let type_params =
    maybe_type_param_decl_to_type_param_defs(function.type_params.as_deref());

  let has_body = function.body.is_some();

  let decorators = decorators_to_defs(parsed_source, &function.decorators);

  FunctionDef {
    params,
    return_type: maybe_return_type,
    has_body,
    is_async: function.is_async,
    is_generator: function.is_generator,
    type_params,
    decorators,
  }
}

pub fn get_doc_for_fn_decl(
  parsed_source: &ParsedSource,
  fn_decl: &deno_ast::swc::ast::FnDecl,
) -> (String, FunctionDef) {
  let name = fn_decl.ident.sym.to_string();
  let fn_def = function_to_function_def(parsed_source, &fn_decl.function);
  (name, fn_def)
}

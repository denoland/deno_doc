// Copyright 2020-2021 the Deno authors. All rights reserved. MIT license.
use crate::params::pat_to_param_def;
use crate::parser::DocParser;
use crate::ts_type::ts_type_ann_to_def;
use crate::ts_type::TsTypeDef;
use crate::ts_type_param::maybe_type_param_decl_to_type_param_defs;
use crate::ts_type_param::TsTypeParamDef;
use crate::ParamDef;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct FunctionDef {
  pub params: Vec<ParamDef>,
  pub return_type: Option<TsTypeDef>,
  pub is_async: bool,
  pub is_generator: bool,
  pub type_params: Vec<TsTypeParamDef>,
  // TODO(bartlomieju): decorators
}

pub fn function_to_function_def(
  doc_parser: &DocParser,
  function: &swc_ecmascript::ast::Function,
) -> FunctionDef {
  let mut params = vec![];

  for param in &function.params {
    let param_def =
      pat_to_param_def(&param.pat, Some(&doc_parser.ast_parser.source_map));
    params.push(param_def);
  }

  let maybe_return_type = function
    .return_type
    .as_ref()
    .map(|rt| ts_type_ann_to_def(rt));

  let type_params =
    maybe_type_param_decl_to_type_param_defs(function.type_params.as_ref());

  FunctionDef {
    params,
    return_type: maybe_return_type,
    is_async: function.is_async,
    is_generator: function.is_generator,
    type_params,
  }
}

pub fn get_doc_for_fn_decl(
  doc_parser: &DocParser,
  fn_decl: &swc_ecmascript::ast::FnDecl,
) -> (String, FunctionDef) {
  let name = fn_decl.ident.sym.to_string();
  let fn_def = function_to_function_def(&doc_parser, &fn_decl.function);
  (name, fn_def)
}

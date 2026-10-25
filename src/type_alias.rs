// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
use crate::ts_type::TsTypeDef;
use crate::ts_type_param::TsTypeParamDef;
use crate::ts_type_param::maybe_type_param_decl_to_type_param_defs;
use deno_ast::oxc::ast::ast::TSTypeAliasDeclaration;
use deno_graph::symbols::EsModuleInfo;
use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct TypeAliasDef {
  pub ts_type: TsTypeDef,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub type_params: Box<[TsTypeParamDef]>,
}

pub fn get_doc_for_ts_type_alias_decl(
  module_info: &EsModuleInfo,
  type_alias_decl: &TSTypeAliasDeclaration,
) -> TypeAliasDef {
  let ts_type = TsTypeDef::new(module_info, &type_alias_decl.type_annotation);
  let type_params = maybe_type_param_decl_to_type_param_defs(
    module_info,
    type_alias_decl.type_parameters.as_deref(),
  );

  TypeAliasDef {
    ts_type,
    type_params,
  }
}

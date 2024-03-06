// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
use crate::ts_type::TsTypeDef;
use crate::ts_type_param::maybe_type_param_decl_to_type_param_defs;
use crate::ts_type_param::TsTypeParamDef;
use deno_ast::ParsedSource;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct TypeAliasDef {
  pub ts_type: TsTypeDef,
  pub type_params: Vec<TsTypeParamDef>,
}

pub fn get_doc_for_ts_type_alias_decl(
  parsed_source: &ParsedSource,
  type_alias_decl: &deno_ast::swc::ast::TsTypeAliasDecl,
) -> (String, TypeAliasDef) {
  let alias_name = type_alias_decl.id.sym.to_string();
  let ts_type = TsTypeDef::new(parsed_source, &type_alias_decl.type_ann);
  let type_params = maybe_type_param_decl_to_type_param_defs(
    parsed_source,
    type_alias_decl.type_params.as_deref(),
  );
  let type_alias_def = TypeAliasDef {
    ts_type,
    type_params,
  };

  (alias_name, type_alias_def)
}

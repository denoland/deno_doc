// Copyright 2020-2021 the Deno authors. All rights reserved. MIT license.
use serde::{Deserialize, Serialize};

use crate::ts_type::TsTypeDef;
use crate::ts_type::{infer_simple_ts_type_from_var_decl, ts_type_ann_to_def};

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct VariableDef {
  pub ts_type: Option<TsTypeDef>,
  pub kind: swc_ecmascript::ast::VarDeclKind,
}

// TODO: change this function to return Vec<(String, VariableDef)> as single
// var declaration can have multiple declarators
pub fn get_doc_for_var_decl(
  var_decl: &swc_ecmascript::ast::VarDecl,
) -> (String, VariableDef) {
  assert!(!var_decl.decls.is_empty());
  let var_declarator = var_decl.decls.get(0).unwrap();
  let var_name = match &var_declarator.name {
    swc_ecmascript::ast::Pat::Ident(ident) => ident.id.sym.to_string(),
    _ => "<TODO>".to_string(),
  };

  let maybe_ts_type = match &var_declarator.name {
    swc_ecmascript::ast::Pat::Ident(ident) => {
      ident.type_ann.as_ref().map(|rt| ts_type_ann_to_def(rt))
    }
    _ => None,
  };

  let variable_def = VariableDef {
    ts_type: maybe_ts_type.or_else(|| {
      infer_simple_ts_type_from_var_decl(
        &var_declarator,
        var_decl.kind == swc_ecmascript::ast::VarDeclKind::Const,
      )
    }),
    kind: var_decl.kind,
  };

  (var_name, variable_def)
}

// Copyright 2020-2022 the Deno authors. All rights reserved. MIT license.
use serde::Deserialize;
use serde::Serialize;

use crate::ts_type::infer_simple_ts_type_from_var_decl;
use crate::ts_type::ts_type_ann_to_def;
use crate::ts_type::TsTypeDef;

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct VariableDef {
  pub ts_type: Option<TsTypeDef>,
  pub kind: deno_ast::swc::ast::VarDeclKind,
}

pub fn get_doc_for_var_decl(
  var_decl: &deno_ast::swc::ast::VarDecl,
) -> Vec<(String, VariableDef)> {
  assert!(!var_decl.decls.is_empty());
  let mut items = Vec::<(String, VariableDef)>::new();
  for var_declarator in &var_decl.decls {
    match &var_declarator.name {
      deno_ast::swc::ast::Pat::Ident(ident) => {
        let var_name = ident.id.sym.to_string();
        let maybe_ts_type = ident.type_ann.as_ref().map(ts_type_ann_to_def);
        let variable_def = VariableDef {
          ts_type: maybe_ts_type.or_else(|| {
            infer_simple_ts_type_from_var_decl(
              var_declarator,
              var_decl.kind == deno_ast::swc::ast::VarDeclKind::Const,
            )
          }),
          kind: var_decl.kind,
        };
        items.push((var_name, variable_def));
      }
      _ => (),
    }
  }

  items
}

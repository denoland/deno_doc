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
  previous_nodes: Vec<&crate::DocNode>,
) -> Vec<(String, VariableDef)> {
  assert!(!var_decl.decls.is_empty());
  let mut items = Vec::<(String, VariableDef)>::new();
  for var_declarator in &var_decl.decls {
    let ref_name: Option<String> =
      var_declarator.init.as_ref().and_then(|init| {
        if let deno_ast::swc::ast::Expr::Ident(ident) = &**init {
          Some(ident.sym.to_string())
        } else {
          None
        }
      });

    match &var_declarator.name {
      deno_ast::swc::ast::Pat::Ident(ident) => {
        let var_name = ident.id.sym.to_string();
        let maybe_ts_type =
          ident.type_ann.as_ref().map(ts_type_ann_to_def).or_else(|| {
            if let Some(ref_name) = ref_name {
              previous_nodes.iter().find_map(|prev_node| {
                if prev_node.name == ref_name {
                  prev_node
                    .variable_def
                    .as_ref()
                    .and_then(|prev_def| prev_def.ts_type.clone())
                } else {
                  None
                }
              })
            } else {
              None
            }
          });
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
      deno_ast::swc::ast::Pat::Object(pat) => {
        let ts_type =
          pat.type_ann.as_ref().map(ts_type_ann_to_def).or_else(|| {
            infer_simple_ts_type_from_var_decl(
              var_declarator,
              var_decl.kind == deno_ast::swc::ast::VarDeclKind::Const,
            )
          });

        for prop in &pat.props {
          let name = match prop {
            deno_ast::swc::ast::ObjectPatProp::KeyValue(kv) => {
              crate::params::prop_name_to_string(None, &kv.key)
            }
            deno_ast::swc::ast::ObjectPatProp::Assign(assign) => {
              assign.key.sym.to_string()
            }
            deno_ast::swc::ast::ObjectPatProp::Rest(_) => todo!(),
          };

          let variable_def = VariableDef {
            ts_type: ts_type.clone(), // TODO: get properties of ts_type
            kind: var_decl.kind,
          };
          items.push((name, variable_def));
        }
      }
      _ => (),
    }
  }

  items
}

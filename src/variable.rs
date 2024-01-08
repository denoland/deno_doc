// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use deno_ast::swc::ast::Pat;
use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;
use deno_graph::symbols::EsmModuleInfo;
use deno_graph::symbols::SymbolNodeRef;
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

pub fn get_docs_for_var_declarator(
  module_info: &EsmModuleInfo,
  var_decl: &deno_ast::swc::ast::VarDecl,
  var_declarator: &deno_ast::swc::ast::VarDeclarator,
) -> Vec<(String, VariableDef, Option<SourceRange>)> {
  let mut items = Vec::<(String, VariableDef, Option<SourceRange>)>::new();
  let ref_name: Option<deno_ast::swc::ast::Id> =
    var_declarator.init.as_ref().and_then(|init| {
      if let deno_ast::swc::ast::Expr::Ident(ident) = &**init {
        Some(ident.to_id())
      } else {
        None
      }
    });

  let maybe_ts_type_ann = match &var_declarator.name {
    deno_ast::swc::ast::Pat::Ident(ident) => ident.type_ann.as_ref(),
    deno_ast::swc::ast::Pat::Object(pat) => pat.type_ann.as_ref(),
    _ => None,
  };
  let maybe_ts_type = maybe_ts_type_ann
    .map(|def| ts_type_ann_to_def(def.as_ref()))
    .or_else(|| {
      if let Some(ref_name) = ref_name {
        module_info.symbol_from_swc(&ref_name).and_then(|symbol| {
          // todo(dsherret): it would be better to go to the declaration
          // here, which is somewhat trivial with type tracing.
          for decl in symbol.decls() {
            if let Some(SymbolNodeRef::Var(_, var_declarator, _)) =
              decl.maybe_node()
            {
              if let Pat::Ident(ident) = &var_declarator.name {
                if let Some(type_ann) = &ident.type_ann {
                  return Some(ts_type_ann_to_def(type_ann));
                }
              }
            }
            let maybe_type_ann = infer_simple_ts_type_from_var_decl(
              module_info.source(),
              var_declarator,
              var_decl.kind == deno_ast::swc::ast::VarDeclKind::Const,
            );
            if let Some(type_ann) = maybe_type_ann {
              return Some(type_ann);
            }
          }
          None
        })
      } else {
        None
      }
    })
    .or_else(|| {
      infer_simple_ts_type_from_var_decl(
        module_info.source(),
        var_declarator,
        var_decl.kind == deno_ast::swc::ast::VarDeclKind::Const,
      )
    });

  match &var_declarator.name {
    deno_ast::swc::ast::Pat::Ident(ident) => {
      let var_name = ident.id.sym.to_string();
      let variable_def = VariableDef {
        ts_type: maybe_ts_type,
        kind: var_decl.kind,
      };
      items.push((var_name, variable_def, Some(var_declarator.range())));
    }
    deno_ast::swc::ast::Pat::Object(pat) => {
      for prop in &pat.props {
        let (name, reassign_name, maybe_range) = match prop {
          deno_ast::swc::ast::ObjectPatProp::KeyValue(kv) => (
            crate::params::prop_name_to_string(None, &kv.key),
            match &*kv.value {
              deno_ast::swc::ast::Pat::Ident(ident) => {
                Some(ident.sym.to_string())
              }
              _ => None, // TODO: properly implement
            },
            None,
          ),
          deno_ast::swc::ast::ObjectPatProp::Assign(assign) => {
            (assign.key.sym.to_string(), None, Some(assign.range()))
          }
          deno_ast::swc::ast::ObjectPatProp::Rest(_) => {
            ("".to_string(), None, None)
          } // TODO: properly implement
        };

        let ts_type = maybe_ts_type.as_ref().and_then(|ts_type| {
          ts_type.type_literal.as_ref().and_then(|type_literal| {
            type_literal.properties.iter().find_map(|property| {
              if property.name == name {
                property.ts_type.clone()
              } else {
                None
              }
            })
          })
        });

        let variable_def = VariableDef {
          ts_type,
          kind: var_decl.kind,
        };
        items.push((reassign_name.unwrap_or(name), variable_def, maybe_range));
      }
    }
    _ => (),
  }
  items
}

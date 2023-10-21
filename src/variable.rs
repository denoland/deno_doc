// Copyright 2020-2022 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;

use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;
use serde::Deserialize;
use serde::Serialize;

use crate::ts_type::infer_simple_ts_type_from_var_decl;
use crate::ts_type::ts_type_ann_to_def;
use crate::ts_type::TsTypeDef;
use deno_ast::ParsedSource;

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct VariableDef {
  pub ts_type: Option<TsTypeDef>,
  pub kind: deno_ast::swc::ast::VarDeclKind,
}

pub fn get_docs_for_var_decl(
  parsed_source: &ParsedSource,
  var_decl: &deno_ast::swc::ast::VarDecl,
  symbols: &HashMap<String, crate::DocNode>,
) -> Vec<(String, VariableDef, Option<SourceRange>)> {
  assert!(!var_decl.decls.is_empty());
  let mut items = Vec::<(String, VariableDef, Option<SourceRange>)>::new();
  for var_declarator in &var_decl.decls {
    items.extend(get_docs_for_var_declarator(
      parsed_source,
      var_decl,
      var_declarator,
      symbols,
    ))
  }

  items
}

pub fn get_docs_for_var_declarator(
  parsed_source: &ParsedSource,
  var_decl: &deno_ast::swc::ast::VarDecl,
  var_declarator: &deno_ast::swc::ast::VarDeclarator,
  symbols: &HashMap<String, crate::DocNode>,
) -> Vec<(String, VariableDef, Option<SourceRange>)> {
  let mut items = Vec::<(String, VariableDef, Option<SourceRange>)>::new();
  let ref_name: Option<String> =
    var_declarator.init.as_ref().and_then(|init| {
      if let deno_ast::swc::ast::Expr::Ident(ident) = &**init {
        Some(ident.sym.to_string())
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
        // todo(dsherret): use the hash map to do the lookup?
        symbols.values().find_map(|prev_node| {
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
    })
    .or_else(|| {
      infer_simple_ts_type_from_var_decl(
        parsed_source,
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
      let obj_type = maybe_ts_type.and_then(|type_def| {
        if let Some(type_def) = type_def.type_ref {
          // todo(dsherret): use the hashmap to do the lookup?
          symbols.values().find_map(|prev_node| {
            if prev_node.name == type_def.type_name {
              prev_node
                .type_alias_def
                .as_ref()
                .map(|ts_alias| ts_alias.ts_type.clone())
            } else {
              None
            }
          })
        } else {
          Some(type_def)
        }
      });

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

        let ts_type = obj_type.as_ref().and_then(|ts_type| {
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

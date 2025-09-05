// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use deno_ast::swc::ast::Pat;
use deno_ast::swc::ast::VarDeclKind;
use deno_graph::symbols::EsModuleInfo;
use deno_graph::symbols::SymbolNodeRef;
use serde::Deserialize;
use serde::Serialize;

use crate::ts_type::TsTypeDef;
use crate::ts_type::TsTypeDefKind;
use crate::ts_type::infer_simple_ts_type_from_init;

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct VariableDef {
  pub ts_type: Option<TsTypeDef>,
  pub kind: VarDeclKind,
}

pub fn get_docs_for_var_declarator(
  module_info: &EsModuleInfo,
  var_decl: &deno_ast::swc::ast::VarDecl,
  var_declarator: &deno_ast::swc::ast::VarDeclarator,
) -> Vec<(String, VariableDef)> {
  let mut items = Vec::<(String, VariableDef)>::new();
  let ref_name: Option<deno_ast::swc::ast::Id> =
    var_declarator.init.as_ref().and_then(|init| {
      if let deno_ast::swc::ast::Expr::Ident(ident) = &**init {
        Some(ident.to_id())
      } else {
        None
      }
    });

  let maybe_ts_type_ann = match &var_declarator.name {
    Pat::Ident(ident) => ident.type_ann.as_ref(),
    Pat::Object(pat) => pat.type_ann.as_ref(),
    Pat::Array(pat) => pat.type_ann.as_ref(),
    Pat::Invalid(_) | Pat::Expr(_) | Pat::Rest(_) | Pat::Assign(_) => None,
  };
  let maybe_ts_type = maybe_ts_type_ann
    .map(|def| TsTypeDef::new(module_info, &def.type_ann))
    .or_else(|| {
      if let Some(ref_name) = ref_name {
        module_info.symbol_from_swc(&ref_name).and_then(|symbol| {
          // todo(dsherret): it would be better to go to the declaration
          // here, which is somewhat trivial with type tracing.
          for decl in symbol.decls() {
            if let Some(SymbolNodeRef::Var(_, var_declarator, _)) =
              decl.maybe_node()
              && let Pat::Ident(ident) = &var_declarator.name
              && let Some(type_ann) = &ident.type_ann
            {
              return Some(TsTypeDef::new(module_info, &type_ann.type_ann));
            }
            let maybe_type_ann = infer_simple_ts_type_from_init(
              module_info,
              var_declarator.init.as_deref(),
              var_decl.kind == VarDeclKind::Const,
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
      infer_simple_ts_type_from_init(
        module_info,
        var_declarator.init.as_deref(),
        var_decl.kind == VarDeclKind::Const,
      )
    });

  match &var_declarator.name {
    Pat::Ident(ident) => {
      let var_name = ident.id.sym.to_string();
      let variable_def = VariableDef {
        ts_type: maybe_ts_type,
        kind: var_decl.kind,
      };
      items.push((var_name, variable_def));
    }
    Pat::Object(obj) => get_vars_from_obj_destructuring(
      obj,
      var_decl.kind,
      maybe_ts_type.as_ref(),
      &mut items,
      module_info,
    ),
    Pat::Array(arr) => get_vars_from_array_destructuring(
      arr,
      var_decl.kind,
      maybe_ts_type.as_ref(),
      &mut items,
      module_info,
    ),
    Pat::Expr(_) | Pat::Invalid(_) | Pat::Assign(_) | Pat::Rest(_) => {}
  }
  items
}

fn get_vars_from_obj_destructuring(
  obj: &deno_ast::swc::ast::ObjectPat,
  kind: VarDeclKind,
  maybe_ts_type: Option<&TsTypeDef>,
  items: &mut Vec<(String, VariableDef)>,
  module_info: &EsModuleInfo,
) {
  let mut reached_rest = false;
  for prop in &obj.props {
    assert!(!reached_rest, "object rest is always last");
    let (name, reassign_name, rest_type_ann) = match prop {
      deno_ast::swc::ast::ObjectPatProp::KeyValue(kv) => (
        crate::params::prop_name_to_string(module_info, &kv.key),
        match &*kv.value {
          Pat::Ident(ident) => Some(ident.sym.to_string()),
          Pat::Assign(assign) => {
            let name = match &*assign.left {
              Pat::Ident(ident) => ident.sym.to_string(),
              Pat::Rest(_) => unreachable!("assign cannot have rest"),
              Pat::Assign(_) => unreachable!("rest cannot have assign"),
              Pat::Array(_) | Pat::Object(_) => {
                continue; // TODO(@crowlKats): implement recursive destructuring
              }
              Pat::Invalid(_) | Pat::Expr(_) => continue,
            };

            Some(name)
          }
          Pat::Array(_) | Pat::Object(_) => {
            continue; // TODO(@crowlKats): implement recursive destructuring
          }
          Pat::Rest(_) | Pat::Invalid(_) | Pat::Expr(_) => {
            continue;
          }
        },
        None,
      ),
      deno_ast::swc::ast::ObjectPatProp::Assign(assign) => {
        (assign.key.sym.to_string(), None, None)
      }
      deno_ast::swc::ast::ObjectPatProp::Rest(rest) => {
        reached_rest = true;

        (
          match &*rest.arg {
            Pat::Ident(ident) => ident.sym.to_string(),
            Pat::Rest(_) => unreachable!("rest cannot have rest"),
            Pat::Assign(_) => unreachable!("rest cannot have assign"),
            Pat::Array(_) | Pat::Object(_) => {
              continue; // TODO(@crowlKats): implement recursive destructuring
            }
            Pat::Invalid(_) | Pat::Expr(_) => continue,
          },
          None,
          rest.type_ann.as_ref(),
        )
      }
    };

    let ts_type = if !reached_rest {
      maybe_ts_type.as_ref().and_then(|ts_type| {
        ts_type.type_literal.as_ref().and_then(|type_literal| {
          type_literal.properties.iter().find_map(|property| {
            if property.name == name {
              property.ts_type.clone()
            } else {
              None
            }
          })
        })
      })
    } else {
      rest_type_ann
        .map(|type_ann| TsTypeDef::new(module_info, &type_ann.type_ann))
    };

    let variable_def = VariableDef { ts_type, kind };
    items.push((reassign_name.unwrap_or(name), variable_def));
  }
}

fn get_vars_from_array_destructuring(
  arr: &deno_ast::swc::ast::ArrayPat,
  kind: VarDeclKind,
  maybe_ts_type: Option<&TsTypeDef>,
  items: &mut Vec<(String, VariableDef)>,
  module_info: &EsModuleInfo,
) {
  let mut reached_rest = false;
  for (i, elem) in arr.elems.iter().enumerate() {
    assert!(!reached_rest, "object rest is always last");
    let Some(elem) = elem else {
      continue;
    };

    let (name, rest_type_ann) = match elem {
      Pat::Ident(ident) => (ident.sym.to_string(), None),
      Pat::Rest(rest) => {
        reached_rest = true;
        (
          match &*rest.arg {
            Pat::Ident(ident) => ident.sym.to_string(),
            Pat::Rest(_) => unreachable!("rest cannot have rest"),
            Pat::Assign(_) => unreachable!("rest cannot have assign"),
            Pat::Array(_) | Pat::Object(_) => {
              continue; // TODO(@crowlKats): implement recursive destructuring
            }
            Pat::Invalid(_) | Pat::Expr(_) => continue,
          },
          rest.type_ann.as_ref(),
        )
      }
      Pat::Assign(assign) => {
        let name = match &*assign.left {
          Pat::Ident(ident) => ident.sym.to_string(),
          Pat::Rest(_) => unreachable!("assign cannot have rest"),
          Pat::Assign(_) => unreachable!("rest cannot have assign"),
          Pat::Array(_) | Pat::Object(_) => {
            continue; // TODO(@crowlKats): implement recursive destructuring
          }
          Pat::Invalid(_) | Pat::Expr(_) => continue,
        };

        (name, None)
      }
      Pat::Array(_) | Pat::Object(_) => {
        continue; // TODO(@crowlKats): implement recursive destructuring
      }
      Pat::Invalid(_) | Pat::Expr(_) => continue,
    };

    let ts_type = if !reached_rest {
      maybe_ts_type.and_then(|ts_type| match ts_type.kind.as_ref()? {
        TsTypeDefKind::Array => Some(*ts_type.array.clone().unwrap()),
        TsTypeDefKind::Tuple => ts_type.tuple.as_ref().unwrap().get(i).cloned(),
        _ => None,
      })
    } else {
      rest_type_ann
        .map(|type_ann| TsTypeDef::new(module_info, &type_ann.type_ann))
        .or_else(|| {
          maybe_ts_type.and_then(|ts_type| {
            if ts_type.kind == Some(TsTypeDefKind::Array) {
              Some(ts_type.clone())
            } else {
              None
            }
          })
        })
    };

    let variable_def = VariableDef { ts_type, kind };
    items.push((name, variable_def));
  }
}

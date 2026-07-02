// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use deno_ast::oxc::ast::ast::BindingPattern;
use deno_ast::oxc::ast::ast::VariableDeclaration;
use deno_ast::oxc::ast::ast::VariableDeclarator;
use deno_graph::symbols::EsModuleInfo;
use deno_graph::symbols::SymbolNodeRef;
use serde::Deserialize;
use serde::Serialize;

use crate::ts_type::TsTypeDef;
use crate::ts_type::TsTypeDefKind;
use crate::ts_type::infer_simple_ts_type_from_init;
use crate::util::types::VarDeclKind;

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct VariableDef {
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub ts_type: Option<TsTypeDef>,
  pub kind: VarDeclKind,
}

pub fn get_docs_for_var_declarator(
  module_info: &EsModuleInfo,
  var_decl: &VariableDeclaration,
  var_declarator: &VariableDeclarator,
) -> Vec<(String, VariableDef)> {
  let mut items = Vec::<(String, VariableDef)>::new();
  let kind = VarDeclKind::from(var_decl.kind);

  let ref_ident = var_declarator.init.as_ref().and_then(|init| {
    if let deno_ast::oxc::ast::ast::Expression::Identifier(ident) = init {
      Some(ident)
    } else {
      None
    }
  });

  let maybe_ts_type_ann = var_declarator.type_annotation.as_deref();

  // Type from explicit type annotation only (used for both simple and
  // destructured patterns).
  let explicit_ts_type = maybe_ts_type_ann
    .map(|def| TsTypeDef::new(module_info, &def.type_annotation));

  // Full inferred type (used only for simple identifier patterns, not
  // destructured bindings).
  let maybe_ts_type = explicit_ts_type
    .clone()
    .or_else(|| {
      if let Some(ref_ident) = ref_ident {
        let swc_id = (ref_ident.name.to_string(), 0usize);
        module_info.symbol_from_swc(&swc_id).and_then(|symbol| {
          for decl in symbol.decls() {
            if let Some(SymbolNodeRef::Var(_, ref_var_declarator, _)) =
              decl.maybe_node()
            {
              if let Some(type_ann) = &ref_var_declarator.type_annotation {
                return Some(TsTypeDef::new(
                  module_info,
                  &type_ann.type_annotation,
                ));
              }
              let maybe_type_ann = infer_simple_ts_type_from_init(
                module_info,
                ref_var_declarator.init.as_ref(),
                kind == VarDeclKind::Const,
              );
              if let Some(type_ann) = maybe_type_ann {
                return Some(type_ann);
              }
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
        var_declarator.init.as_ref(),
        kind == VarDeclKind::Const,
      )
    });

  match &var_declarator.id {
    BindingPattern::BindingIdentifier(ident) => {
      let var_name = ident.name.to_string();
      let variable_def = VariableDef {
        ts_type: maybe_ts_type,
        kind,
      };
      items.push((var_name, variable_def));
    }
    BindingPattern::ObjectPattern(obj) => get_vars_from_obj_destructuring(
      obj,
      kind,
      explicit_ts_type.as_ref(),
      &mut items,
      module_info,
    ),
    BindingPattern::ArrayPattern(arr) => get_vars_from_array_destructuring(
      arr,
      kind,
      explicit_ts_type.as_ref(),
      &mut items,
      module_info,
    ),
    BindingPattern::AssignmentPattern(_) => {}
  }
  items
}

fn get_vars_from_obj_destructuring(
  obj: &deno_ast::oxc::ast::ast::ObjectPattern,
  kind: VarDeclKind,
  maybe_ts_type: Option<&TsTypeDef>,
  items: &mut Vec<(String, VariableDef)>,
  module_info: &EsModuleInfo,
) {
  for prop in &obj.properties {
    let (name, reassign_name) = if prop.shorthand {
      let key = match &prop.value {
        BindingPattern::BindingIdentifier(ident) => ident.name.to_string(),
        BindingPattern::AssignmentPattern(assign) => match &assign.left {
          BindingPattern::BindingIdentifier(ident) => ident.name.to_string(),
          _ => continue,
        },
        _ => continue,
      };
      (key, None)
    } else {
      let key = crate::params::prop_name_to_string(module_info, &prop.key);
      let reassign_name = match &prop.value {
        BindingPattern::BindingIdentifier(ident) => {
          Some(ident.name.to_string())
        }
        BindingPattern::AssignmentPattern(assign) => match &assign.left {
          BindingPattern::BindingIdentifier(ident) => {
            Some(ident.name.to_string())
          }
          _ => continue,
        },
        BindingPattern::ArrayPattern(_) | BindingPattern::ObjectPattern(_) => {
          continue; // TODO(@crowlKats): implement recursive destructuring
        }
      };
      (key, reassign_name)
    };

    let ts_type = maybe_ts_type.as_ref().and_then(|ts_type| {
      if let TsTypeDefKind::TypeLiteral(type_literal) = &ts_type.kind {
        type_literal.properties.iter().find_map(|property| {
          if property.name == name {
            property.ts_type.clone()
          } else {
            None
          }
        })
      } else {
        None
      }
    });

    let variable_def = VariableDef { ts_type, kind };
    items.push((reassign_name.unwrap_or(name), variable_def));
  }

  // Handle rest element
  if let Some(rest) = &obj.rest {
    let name = match &rest.argument {
      BindingPattern::BindingIdentifier(ident) => ident.name.to_string(),
      _ => return,
    };
    let variable_def = VariableDef {
      ts_type: None,
      kind,
    };
    items.push((name, variable_def));
  }
}

fn get_vars_from_array_destructuring(
  arr: &deno_ast::oxc::ast::ast::ArrayPattern,
  kind: VarDeclKind,
  maybe_ts_type: Option<&TsTypeDef>,
  items: &mut Vec<(String, VariableDef)>,
  _module_info: &EsModuleInfo,
) {
  for (i, elem) in arr.elements.iter().enumerate() {
    let Some(elem) = elem else {
      continue;
    };

    let name = match elem {
      BindingPattern::BindingIdentifier(ident) => ident.name.to_string(),
      BindingPattern::AssignmentPattern(assign) => match &assign.left {
        BindingPattern::BindingIdentifier(ident) => ident.name.to_string(),
        _ => continue,
      },
      BindingPattern::ArrayPattern(_) | BindingPattern::ObjectPattern(_) => {
        continue; // TODO(@crowlKats): implement recursive destructuring
      }
    };

    let ts_type = maybe_ts_type.and_then(|ts_type| match &ts_type.kind {
      TsTypeDefKind::Array(array) => Some(*array.clone()),
      TsTypeDefKind::Tuple(tuple) => tuple.get(i).cloned(),
      _ => None,
    });

    let variable_def = VariableDef { ts_type, kind };
    items.push((name, variable_def));
  }

  // Handle rest element
  if let Some(rest) = &arr.rest {
    let name = match &rest.argument {
      BindingPattern::BindingIdentifier(ident) => ident.name.to_string(),
      _ => return,
    };

    let ts_type = maybe_ts_type.and_then(|ts_type| {
      if matches!(ts_type.kind, TsTypeDefKind::Array(_)) {
        Some(ts_type.clone())
      } else {
        None
      }
    });

    let variable_def = VariableDef { ts_type, kind };
    items.push((name, variable_def));
  }
}

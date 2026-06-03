// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::ParamDef;
use crate::decorators::DecoratorDef;
use crate::params::param_to_param_def;
use crate::ts_type::TsTypeDef;
use crate::ts_type_param::TsTypeParamDef;
use crate::ts_type_param::maybe_type_param_decl_to_type_param_defs;
use crate::util::swc::is_false;
use deno_ast::oxc::ast::ast::Function;
use deno_ast::oxc::ast::ast::ReturnStatement;
use deno_ast::oxc::ast::ast::Statement;
use deno_graph::symbols::EsModuleInfo;
use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct FunctionDef {
  #[serde(skip_serializing_if = "Option::is_none", default)]
  /// set when the function is a default export and has a name in its declaration
  pub def_name: Option<String>,
  pub params: Vec<ParamDef>,
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub return_type: Option<TsTypeDef>,
  #[serde(skip_serializing_if = "is_false", default)]
  pub has_body: bool,
  #[serde(skip_serializing_if = "is_false", default)]
  pub is_async: bool,
  #[serde(skip_serializing_if = "is_false", default)]
  pub is_generator: bool,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub type_params: Box<[TsTypeParamDef]>,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub decorators: Box<[DecoratorDef]>,
}

pub fn function_to_function_def(
  module_info: &EsModuleInfo,
  function: &Function,
  def_name: Option<String>,
) -> FunctionDef {
  let mut params = function
    .params
    .items
    .iter()
    .map(|param| param_to_param_def(module_info, param))
    .collect::<Vec<_>>();
  if let Some(rest) = &function.params.rest {
    params.push(crate::params::rest_element_to_param_def(
      module_info,
      &rest.rest,
      rest.type_annotation.as_deref(),
    ));
  }

  let maybe_return_type =
    match function.return_type.as_ref().map(|return_type| {
      TsTypeDef::new(module_info, &return_type.type_annotation)
    }) {
      Some(return_type) => Some(return_type),
      None
        if !function.generator
          && function.body.is_some()
          && get_return_stmt_with_arg_from_function(function).is_none() =>
      {
        if function.r#async {
          Some(TsTypeDef {
            repr: "Promise".to_string(),
            kind: crate::ts_type::TsTypeDefKind::TypeRef(
              crate::ts_type::TsTypeRefDef {
                type_params: Some(Box::new([TsTypeDef::keyword("void")])),
                type_name: "Promise".to_string(),
                resolution: None,
              },
            ),
          })
        } else {
          Some(TsTypeDef::keyword("void"))
        }
      }
      None => None,
    };

  let type_params = maybe_type_param_decl_to_type_param_defs(
    module_info,
    function.type_parameters.as_deref(),
  );

  let has_body = function.body.is_some();

  let decorators = Box::new([]);

  FunctionDef {
    def_name,
    params,
    return_type: maybe_return_type,
    has_body,
    is_async: function.r#async,
    is_generator: function.generator,
    type_params,
    decorators,
  }
}

pub fn arrow_to_function_def(
  module_info: &EsModuleInfo,
  arrow: &deno_ast::oxc::ast::ast::ArrowFunctionExpression,
) -> FunctionDef {
  let mut params = arrow
    .params
    .items
    .iter()
    .map(|param| param_to_param_def(module_info, param))
    .collect::<Vec<_>>();
  if let Some(rest) = &arrow.params.rest {
    params.push(crate::params::rest_element_to_param_def(
      module_info,
      &rest.rest,
      rest.type_annotation.as_deref(),
    ));
  }

  let maybe_return_type = arrow.return_type.as_ref().map(|return_type| {
    TsTypeDef::new(module_info, &return_type.type_annotation)
  });

  let type_params = maybe_type_param_decl_to_type_param_defs(
    module_info,
    arrow.type_parameters.as_deref(),
  );

  FunctionDef {
    def_name: None,
    params,
    return_type: maybe_return_type,
    has_body: true,
    is_async: arrow.r#async,
    is_generator: false,
    type_params,
    decorators: Box::new([]),
  }
}

pub fn get_doc_for_fn_decl(
  module_info: &EsModuleInfo,
  function: &Function,
) -> FunctionDef {
  function_to_function_def(module_info, function, None)
}

fn get_return_stmt_with_arg_from_function<'a>(
  func: &'a Function<'a>,
) -> Option<&'a ReturnStatement<'a>> {
  let body = func.body.as_ref()?;
  let stmt = get_return_stmt_with_arg_from_stmts(&body.statements)?;
  debug_assert!(stmt.argument.is_some());
  Some(stmt)
}

fn get_return_stmt_with_arg_from_stmts<'a>(
  stmts: &'a [Statement<'a>],
) -> Option<&'a ReturnStatement<'a>> {
  for stmt in stmts {
    if let Some(return_stmt) = get_return_stmt_with_arg_from_stmt(stmt) {
      return Some(return_stmt);
    }
  }

  None
}

fn get_return_stmt_with_arg_from_stmt<'a>(
  stmt: &'a Statement<'a>,
) -> Option<&'a ReturnStatement<'a>> {
  match stmt {
    Statement::BlockStatement(n) => {
      get_return_stmt_with_arg_from_stmts(&n.body)
    }
    Statement::WithStatement(n) => get_return_stmt_with_arg_from_stmt(&n.body),
    Statement::ReturnStatement(n) => {
      if n.argument.is_none() {
        None
      } else {
        Some(n)
      }
    }
    Statement::LabeledStatement(n) => {
      get_return_stmt_with_arg_from_stmt(&n.body)
    }
    Statement::IfStatement(n) => {
      get_return_stmt_with_arg_from_stmt(&n.consequent)
    }
    Statement::SwitchStatement(n) => n
      .cases
      .iter()
      .find_map(|case| get_return_stmt_with_arg_from_stmts(&case.consequent)),
    Statement::TryStatement(n) => {
      get_return_stmt_with_arg_from_stmts(&n.block.body)
        .or_else(|| {
          n.handler
            .as_ref()
            .and_then(|h| get_return_stmt_with_arg_from_stmts(&h.body.body))
        })
        .or_else(|| {
          n.finalizer
            .as_ref()
            .and_then(|f| get_return_stmt_with_arg_from_stmts(&f.body))
        })
    }
    Statement::WhileStatement(n) => get_return_stmt_with_arg_from_stmt(&n.body),
    Statement::DoWhileStatement(n) => {
      get_return_stmt_with_arg_from_stmt(&n.body)
    }
    Statement::ForStatement(n) => get_return_stmt_with_arg_from_stmt(&n.body),
    Statement::ForInStatement(n) => get_return_stmt_with_arg_from_stmt(&n.body),
    Statement::ForOfStatement(n) => get_return_stmt_with_arg_from_stmt(&n.body),
    Statement::BreakStatement(_)
    | Statement::ContinueStatement(_)
    | Statement::ThrowStatement(_)
    | Statement::DebuggerStatement(_)
    | Statement::ExpressionStatement(_)
    | Statement::EmptyStatement(_) => None,
    _ => None,
  }
}

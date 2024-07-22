// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::decorators::decorators_to_defs;
use crate::decorators::DecoratorDef;
use crate::params::param_to_param_def;
use crate::ts_type::TsTypeDef;
use crate::ts_type_param::maybe_type_param_decl_to_type_param_defs;
use crate::ts_type_param::TsTypeParamDef;
use crate::util::swc::is_false;
use crate::ParamDef;
use deno_ast::swc::ast::ReturnStmt;
use deno_ast::swc::ast::Stmt;
use deno_ast::ParsedSource;
use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct FunctionDef {
  #[serde(skip_serializing_if = "Option::is_none", default)]
  /// set when the function is a default export and has a name in its declaration
  pub def_name: Option<String>,
  pub params: Vec<ParamDef>,
  pub return_type: Option<TsTypeDef>,
  #[serde(skip_serializing_if = "is_false", default)]
  pub has_body: bool,
  pub is_async: bool,
  pub is_generator: bool,
  pub type_params: Box<[TsTypeParamDef]>,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub decorators: Box<[DecoratorDef]>,
}

pub fn function_to_function_def(
  parsed_source: &ParsedSource,
  function: &deno_ast::swc::ast::Function,
  def_name: Option<String>,
) -> FunctionDef {
  let params = function
    .params
    .iter()
    .map(|param| param_to_param_def(parsed_source, param))
    .collect();

  let maybe_return_type = match function
    .return_type
    .as_deref()
    .map(|return_type| TsTypeDef::new(parsed_source, &return_type.type_ann))
  {
    Some(return_type) => Some(return_type),
    None
      if !function.is_generator
        && function.body.is_some()
        && get_return_stmt_with_arg_from_function(function).is_none() =>
    {
      if function.is_async {
        Some(TsTypeDef {
          repr: "Promise".to_string(),
          kind: Some(crate::ts_type::TsTypeDefKind::TypeRef),
          type_ref: Some(crate::ts_type::TsTypeRefDef {
            type_params: Some(Box::new([TsTypeDef::keyword("void")])),
            type_name: "Promise".to_string(),
          }),
          ..Default::default()
        })
      } else {
        Some(TsTypeDef::keyword("void"))
      }
    }
    None => None,
  };

  let type_params = maybe_type_param_decl_to_type_param_defs(
    parsed_source,
    function.type_params.as_deref(),
  );

  let has_body = function.body.is_some();

  let decorators = decorators_to_defs(parsed_source, &function.decorators);

  FunctionDef {
    def_name,
    params,
    return_type: maybe_return_type,
    has_body,
    is_async: function.is_async,
    is_generator: function.is_generator,
    type_params,
    decorators,
  }
}

pub fn get_doc_for_fn_decl(
  parsed_source: &ParsedSource,
  fn_decl: &deno_ast::swc::ast::FnDecl,
) -> (String, FunctionDef) {
  let name = fn_decl.ident.sym.to_string();
  let fn_def = function_to_function_def(parsed_source, &fn_decl.function, None);
  (name, fn_def)
}

fn get_return_stmt_with_arg_from_function(
  func: &deno_ast::swc::ast::Function,
) -> Option<&ReturnStmt> {
  let body = func.body.as_ref()?;
  let stmt = get_return_stmt_with_arg_from_stmts(&body.stmts)?;
  debug_assert!(stmt.arg.is_some());
  Some(stmt)
}

fn get_return_stmt_with_arg_from_stmts(stmts: &[Stmt]) -> Option<&ReturnStmt> {
  for stmt in stmts {
    if let Some(return_stmt) = get_return_stmt_with_arg_from_stmt(stmt) {
      return Some(return_stmt);
    }
  }

  None
}

fn get_return_stmt_with_arg_from_stmt(stmt: &Stmt) -> Option<&ReturnStmt> {
  match stmt {
    Stmt::Block(n) => get_return_stmt_with_arg_from_stmts(&n.stmts),
    Stmt::With(n) => get_return_stmt_with_arg_from_stmt(&n.body),
    Stmt::Return(n) => {
      if n.arg.is_none() {
        None
      } else {
        Some(n)
      }
    }
    Stmt::Labeled(n) => get_return_stmt_with_arg_from_stmt(&n.body),
    Stmt::If(n) => get_return_stmt_with_arg_from_stmt(&n.cons),
    Stmt::Switch(n) => n
      .cases
      .iter()
      .find_map(|case| get_return_stmt_with_arg_from_stmts(&case.cons)),
    Stmt::Try(n) => get_return_stmt_with_arg_from_stmts(&n.block.stmts)
      .or_else(|| {
        n.handler
          .as_ref()
          .and_then(|h| get_return_stmt_with_arg_from_stmts(&h.body.stmts))
      })
      .or_else(|| {
        n.finalizer
          .as_ref()
          .and_then(|f| get_return_stmt_with_arg_from_stmts(&f.stmts))
      }),
    Stmt::While(n) => get_return_stmt_with_arg_from_stmt(&n.body),
    Stmt::DoWhile(n) => get_return_stmt_with_arg_from_stmt(&n.body),
    Stmt::For(n) => get_return_stmt_with_arg_from_stmt(&n.body),
    Stmt::ForIn(n) => get_return_stmt_with_arg_from_stmt(&n.body),
    Stmt::ForOf(n) => get_return_stmt_with_arg_from_stmt(&n.body),
    Stmt::Break(_)
    | Stmt::Continue(_)
    | Stmt::Throw(_)
    | Stmt::Debugger(_)
    | Stmt::Decl(_)
    | Stmt::Expr(_)
    | Stmt::Empty(_) => None,
  }
}

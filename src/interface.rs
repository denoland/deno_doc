// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use deno_ast::ParsedSource;
use deno_ast::SourceRangedForSpanned;
use serde::Deserialize;
use serde::Serialize;

use crate::params::ts_fn_param_to_param_def;
use crate::ts_type::CallSignatureDef;
use crate::ts_type::ConstructorDef;
use crate::ts_type::IndexSignatureDef;
use crate::ts_type::MethodDef;
use crate::ts_type::PropertyDef;
use crate::ts_type::TsTypeDef;
use crate::ts_type_param::maybe_type_param_decl_to_type_param_defs;
use crate::ts_type_param::TsTypeParamDef;
use crate::util::swc::get_location;
use crate::util::swc::js_doc_for_range;

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct InterfaceDef {
  #[serde(skip_serializing_if = "Option::is_none", default)]
  /// set when the interface is a default export
  pub def_name: Option<String>,
  pub extends: Vec<TsTypeDef>,
  #[serde(default)]
  pub constructors: Vec<ConstructorDef>,
  pub methods: Vec<MethodDef>,
  pub properties: Vec<PropertyDef>,
  pub call_signatures: Vec<CallSignatureDef>,
  pub index_signatures: Vec<IndexSignatureDef>,
  pub type_params: Vec<TsTypeParamDef>,
}

pub fn expr_to_name(expr: &deno_ast::swc::ast::Expr) -> String {
  use deno_ast::swc::ast::Expr::*;
  use deno_ast::swc::ast::MemberProp;

  match expr {
    Ident(ident) => ident.sym.to_string(),
    Member(member_expr) => {
      let left = expr_to_name(&member_expr.obj);
      let right = match &member_expr.prop {
        MemberProp::Ident(ident) => format!(".{}", ident.sym),
        MemberProp::Computed(_) | MemberProp::PrivateName(_) => {
          "[UNSUPPORTED]".to_string()
        }
      };
      format!("[{}{}]", left, right)
    }
    Lit(lit) => {
      use deno_ast::swc::ast::BigInt;
      use deno_ast::swc::ast::Bool;
      use deno_ast::swc::ast::JSXText;
      use deno_ast::swc::ast::Lit;
      use deno_ast::swc::ast::Number;
      use deno_ast::swc::ast::Regex;
      use deno_ast::swc::ast::Str;
      match lit {
        Lit::Str(Str { ref value, .. }) => value.to_string(),
        Lit::Bool(Bool { ref value, .. }) => {
          let str_val = if *value { "true" } else { "false" };
          str_val.to_string()
        }
        Lit::Null(_) => "null".to_string(),
        Lit::Num(Number { ref value, .. }) => value.to_string(),
        Lit::BigInt(BigInt { ref value, .. }) => value.to_string(),
        Lit::Regex(Regex { ref exp, .. }) => format!("/{}/", exp),
        Lit::JSXText(JSXText { ref raw, .. }) => raw.to_string(),
      }
    }
    _ => "[UNSUPPORTED]".to_string(),
  }
}

pub fn get_doc_for_ts_interface_decl(
  parsed_source: &ParsedSource,
  interface_decl: &deno_ast::swc::ast::TsInterfaceDecl,
  def_name: Option<String>,
) -> (String, InterfaceDef) {
  let interface_name = interface_decl.id.sym.to_string();

  let mut constructors = vec![];
  let mut methods = vec![];
  let mut properties = vec![];
  let mut call_signatures = vec![];
  let mut index_signatures = vec![];

  for type_element in &interface_decl.body.body {
    use deno_ast::swc::ast::TsTypeElement::*;

    match &type_element {
      TsMethodSignature(ts_method_sig) => {
        if let Some(method_js_doc) =
          js_doc_for_range(parsed_source, &ts_method_sig.range())
        {
          let mut params = vec![];

          for param in &ts_method_sig.params {
            let param_def = ts_fn_param_to_param_def(parsed_source, param);
            params.push(param_def);
          }

          let name = expr_to_name(&ts_method_sig.key);

          let maybe_return_type = ts_method_sig
            .type_ann
            .as_deref()
            .map(|type_ann| TsTypeDef::new(parsed_source, &type_ann.type_ann));

          let type_params = maybe_type_param_decl_to_type_param_defs(
            parsed_source,
            ts_method_sig.type_params.as_deref(),
          );

          let method_def = MethodDef {
            name,
            kind: deno_ast::swc::ast::MethodKind::Method,
            js_doc: method_js_doc,
            location: get_location(parsed_source, ts_method_sig.start()),
            computed: ts_method_sig.computed,
            optional: ts_method_sig.optional,
            params,
            return_type: maybe_return_type,
            type_params,
          };
          methods.push(method_def);
        }
      }
      TsGetterSignature(ts_getter_sig) => {
        if let Some(method_js_doc) =
          js_doc_for_range(parsed_source, &ts_getter_sig.range())
        {
          let name = expr_to_name(&ts_getter_sig.key);

          let maybe_return_type = ts_getter_sig
            .type_ann
            .as_deref()
            .map(|type_ann| TsTypeDef::new(parsed_source, &type_ann.type_ann));

          let method_def = MethodDef {
            name,
            kind: deno_ast::swc::ast::MethodKind::Getter,
            js_doc: method_js_doc,
            location: get_location(parsed_source, ts_getter_sig.start()),
            computed: ts_getter_sig.computed,
            optional: false,
            params: vec![],
            return_type: maybe_return_type,
            type_params: vec![],
          };
          methods.push(method_def);
        }
      }
      TsSetterSignature(ts_setter_sig) => {
        if let Some(method_js_doc) =
          js_doc_for_range(parsed_source, &ts_setter_sig.range())
        {
          let name = expr_to_name(&ts_setter_sig.key);

          let param_def =
            ts_fn_param_to_param_def(parsed_source, &ts_setter_sig.param);
          let params = vec![param_def];

          let method_def = MethodDef {
            name,
            kind: deno_ast::swc::ast::MethodKind::Setter,
            js_doc: method_js_doc,
            location: get_location(parsed_source, ts_setter_sig.start()),
            computed: ts_setter_sig.computed,
            optional: false,
            params,
            return_type: None,
            type_params: vec![],
          };
          methods.push(method_def);
        }
      }
      TsPropertySignature(ts_prop_sig) => {
        if let Some(prop_js_doc) =
          js_doc_for_range(parsed_source, &ts_prop_sig.range())
        {
          let name = expr_to_name(&ts_prop_sig.key);

          let ts_type = ts_prop_sig
            .type_ann
            .as_deref()
            .map(|type_ann| TsTypeDef::new(parsed_source, &type_ann.type_ann));

          let type_params = maybe_type_param_decl_to_type_param_defs(
            parsed_source,
            None,
          );

          let prop_def = PropertyDef {
            name,
            js_doc: prop_js_doc,
            location: get_location(parsed_source, ts_prop_sig.start()),
            params: vec![],
            ts_type,
            readonly: ts_prop_sig.readonly,
            computed: ts_prop_sig.computed,
            optional: ts_prop_sig.optional,
            type_params,
          };
          properties.push(prop_def);
        }
      }
      TsCallSignatureDecl(ts_call_sig) => {
        if let Some(call_sig_js_doc) =
          js_doc_for_range(parsed_source, &ts_call_sig.range())
        {
          let mut params = vec![];
          for param in &ts_call_sig.params {
            let param_def = ts_fn_param_to_param_def(parsed_source, param);
            params.push(param_def);
          }

          let ts_type = ts_call_sig
            .type_ann
            .as_deref()
            .map(|type_ann| TsTypeDef::new(parsed_source, &type_ann.type_ann));

          let type_params = maybe_type_param_decl_to_type_param_defs(
            parsed_source,
            ts_call_sig.type_params.as_deref(),
          );

          let call_sig_def = CallSignatureDef {
            js_doc: call_sig_js_doc,
            location: get_location(parsed_source, ts_call_sig.start()),
            params,
            ts_type,
            type_params,
          };
          call_signatures.push(call_sig_def);
        }
      }
      TsIndexSignature(ts_index_sig) => {
        if let Some(js_doc) =
          js_doc_for_range(parsed_source, &ts_index_sig.range())
        {
          let mut params = vec![];
          for param in &ts_index_sig.params {
            let param_def = ts_fn_param_to_param_def(parsed_source, param);
            params.push(param_def);
          }

          let ts_type = ts_index_sig
            .type_ann
            .as_ref()
            .map(|rt| TsTypeDef::new(parsed_source, &rt.type_ann));

          let index_sig_def = IndexSignatureDef {
            location: get_location(parsed_source, ts_index_sig.start()),
            js_doc,
            readonly: ts_index_sig.readonly,
            params,
            ts_type,
          };
          index_signatures.push(index_sig_def);
        }
      }
      TsConstructSignatureDecl(ts_construct_sig) => {
        if let Some(js_doc) =
          js_doc_for_range(parsed_source, &ts_construct_sig.range())
        {
          let mut params = vec![];

          for param in &ts_construct_sig.params {
            let param_def = ts_fn_param_to_param_def(parsed_source, param);
            params.push(param_def);
          }

          let type_params = maybe_type_param_decl_to_type_param_defs(
            parsed_source,
            ts_construct_sig.type_params.as_deref(),
          );

          let maybe_return_type = ts_construct_sig
            .type_ann
            .as_ref()
            .map(|rt| TsTypeDef::new(parsed_source, &rt.type_ann));

          let construct_sig_def = ConstructorDef {
            js_doc,
            location: get_location(parsed_source, ts_construct_sig.start()),
            params,
            return_type: maybe_return_type,
            type_params,
          };

          constructors.push(construct_sig_def);
        }
      }
    }
  }

  let type_params = maybe_type_param_decl_to_type_param_defs(
    parsed_source,
    interface_decl.type_params.as_deref(),
  );

  let extends = interface_decl
    .extends
    .iter()
    .map(|expr| TsTypeDef::ts_expr_with_type_args(parsed_source, expr))
    .collect::<Vec<TsTypeDef>>();

  let interface_def = InterfaceDef {
    def_name,
    extends,
    constructors,
    methods,
    properties,
    call_signatures,
    index_signatures,
    type_params,
  };

  (interface_name, interface_def)
}

// Copyright 2020-2021 the Deno authors. All rights reserved. MIT license.

use serde::Deserialize;
use serde::Serialize;

use crate::colors;
use crate::display::{display_optional, display_readonly, SliceDisplayer};
use crate::function::FunctionDef;
use crate::js_doc::JsDoc;
use crate::params::ts_fn_param_to_param_def;
use crate::parser::DocParser;
use crate::ts_type::ts_type_ann_to_def;
use crate::ts_type::TsTypeDef;
use crate::ts_type_param::maybe_type_param_decl_to_type_param_defs;
use crate::ts_type_param::TsTypeParamDef;
use crate::variable::VariableDef;
use crate::DocNode;
use crate::Location;
use crate::ParamDef;

use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct InterfaceMethodDef {
  pub name: String,
  pub kind: swc_ecmascript::ast::MethodKind,
  pub location: Location,
  #[serde(skip_serializing_if = "JsDoc::is_empty")]
  pub js_doc: JsDoc,
  pub optional: bool,
  pub params: Vec<ParamDef>,
  pub return_type: Option<TsTypeDef>,
  pub type_params: Vec<TsTypeParamDef>,
}

impl From<InterfaceMethodDef> for DocNode {
  fn from(def: InterfaceMethodDef) -> DocNode {
    DocNode::function(
      def.name,
      def.location,
      def.js_doc,
      FunctionDef {
        params: def.params,
        return_type: def.return_type,
        is_async: false,
        is_generator: false,
        type_params: def.type_params,
      },
    )
  }
}

impl Display for InterfaceMethodDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{}{}({})",
      colors::bold(&self.name),
      display_optional(self.optional),
      SliceDisplayer::new(&self.params, ", ", false),
    )?;
    if let Some(return_type) = &self.return_type {
      write!(f, ": {}", return_type)?;
    }
    Ok(())
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct InterfacePropertyDef {
  pub name: String,
  pub location: Location,
  #[serde(skip_serializing_if = "JsDoc::is_empty")]
  pub js_doc: JsDoc,
  pub params: Vec<ParamDef>,
  pub computed: bool,
  pub optional: bool,
  pub ts_type: Option<TsTypeDef>,
  pub type_params: Vec<TsTypeParamDef>,
}

impl From<InterfacePropertyDef> for DocNode {
  fn from(def: InterfacePropertyDef) -> DocNode {
    DocNode::variable(
      def.name,
      def.location,
      def.js_doc,
      VariableDef {
        ts_type: def.ts_type,
        kind: swc_ecmascript::ast::VarDeclKind::Const,
      },
    )
  }
}

impl Display for InterfacePropertyDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{}{}",
      colors::bold(&self.name),
      display_optional(self.optional),
    )?;
    if let Some(ts_type) = &self.ts_type {
      write!(f, ": {}", ts_type)?;
    }
    Ok(())
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct InterfaceIndexSignatureDef {
  pub readonly: bool,
  pub params: Vec<ParamDef>,
  pub ts_type: Option<TsTypeDef>,
}

impl Display for InterfaceIndexSignatureDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{}[{}]",
      display_readonly(self.readonly),
      SliceDisplayer::new(&self.params, ", ", false)
    )?;
    if let Some(ts_type) = &self.ts_type {
      write!(f, ": {}", ts_type)?;
    }
    Ok(())
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct InterfaceCallSignatureDef {
  pub location: Location,
  #[serde(skip_serializing_if = "JsDoc::is_empty")]
  pub js_doc: JsDoc,
  pub params: Vec<ParamDef>,
  pub ts_type: Option<TsTypeDef>,
  pub type_params: Vec<TsTypeParamDef>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct InterfaceDef {
  pub extends: Vec<TsTypeDef>,
  pub methods: Vec<InterfaceMethodDef>,
  pub properties: Vec<InterfacePropertyDef>,
  pub call_signatures: Vec<InterfaceCallSignatureDef>,
  pub index_signatures: Vec<InterfaceIndexSignatureDef>,
  pub type_params: Vec<TsTypeParamDef>,
}

pub fn expr_to_name(expr: &swc_ecmascript::ast::Expr) -> String {
  use swc_ecmascript::ast::Expr::*;
  use swc_ecmascript::ast::ExprOrSuper::*;

  match expr {
    Ident(ident) => ident.sym.to_string(),
    Member(member_expr) => {
      let left = match &member_expr.obj {
        Super(_) => "super".to_string(),
        Expr(boxed_expr) => expr_to_name(&*boxed_expr),
      };
      let right = expr_to_name(&*member_expr.prop);
      format!("[{}.{}]", left, right)
    }
    Lit(lit) => {
      use swc_ecmascript::ast::BigInt;
      use swc_ecmascript::ast::Bool;
      use swc_ecmascript::ast::JSXText;
      use swc_ecmascript::ast::Lit;
      use swc_ecmascript::ast::Number;
      use swc_ecmascript::ast::Regex;
      use swc_ecmascript::ast::Str;
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
    _ => "<TODO>".to_string(),
  }
}

pub fn get_doc_for_ts_interface_decl(
  doc_parser: &DocParser,
  interface_decl: &swc_ecmascript::ast::TsInterfaceDecl,
) -> (String, InterfaceDef) {
  let interface_name = interface_decl.id.sym.to_string();

  let mut methods = vec![];
  let mut properties = vec![];
  let mut call_signatures = vec![];
  let mut index_signatures = vec![];

  for type_element in &interface_decl.body.body {
    use swc_ecmascript::ast::TsTypeElement::*;

    match &type_element {
      TsMethodSignature(ts_method_sig) => {
        let method_js_doc = doc_parser.js_doc_for_span(ts_method_sig.span);

        let mut params = vec![];

        for param in &ts_method_sig.params {
          let param_def = ts_fn_param_to_param_def(
            param,
            Some(&doc_parser.ast_parser.source_map),
          );
          params.push(param_def);
        }

        let name = expr_to_name(&*ts_method_sig.key);

        let maybe_return_type = ts_method_sig
          .type_ann
          .as_ref()
          .map(|rt| ts_type_ann_to_def(rt));

        let type_params = maybe_type_param_decl_to_type_param_defs(
          ts_method_sig.type_params.as_ref(),
        );

        let method_def = InterfaceMethodDef {
          name,
          kind: swc_ecmascript::ast::MethodKind::Method,
          js_doc: method_js_doc,
          location: doc_parser
            .ast_parser
            .get_span_location(ts_method_sig.span)
            .into(),
          optional: ts_method_sig.optional,
          params,
          return_type: maybe_return_type,
          type_params,
        };
        methods.push(method_def);
      }
      TsGetterSignature(ts_getter_sig) => {
        let method_js_doc = doc_parser.js_doc_for_span(ts_getter_sig.span);
        let name = expr_to_name(&*ts_getter_sig.key);

        let maybe_return_type = ts_getter_sig
          .type_ann
          .as_ref()
          .map(|rt| ts_type_ann_to_def(rt));

        let method_def = InterfaceMethodDef {
          name,
          kind: swc_ecmascript::ast::MethodKind::Getter,
          js_doc: method_js_doc,
          location: doc_parser
            .ast_parser
            .get_span_location(ts_getter_sig.span)
            .into(),
          optional: ts_getter_sig.optional,
          params: vec![],
          return_type: maybe_return_type,
          type_params: vec![],
        };
        methods.push(method_def);
      }
      TsSetterSignature(ts_setter_sig) => {
        let method_js_doc = doc_parser.js_doc_for_span(ts_setter_sig.span);

        let name = expr_to_name(&*ts_setter_sig.key);

        let param_def = ts_fn_param_to_param_def(
          &ts_setter_sig.param,
          Some(&doc_parser.ast_parser.source_map),
        );
        let params = vec![param_def];

        let method_def = InterfaceMethodDef {
          name,
          kind: swc_ecmascript::ast::MethodKind::Setter,
          js_doc: method_js_doc,
          location: doc_parser
            .ast_parser
            .get_span_location(ts_setter_sig.span)
            .into(),
          optional: ts_setter_sig.optional,
          params,
          return_type: None,
          type_params: vec![],
        };
        methods.push(method_def);
      }
      TsPropertySignature(ts_prop_sig) => {
        let prop_js_doc = doc_parser.js_doc_for_span(ts_prop_sig.span);
        let name = expr_to_name(&*ts_prop_sig.key);

        let mut params = vec![];

        for param in &ts_prop_sig.params {
          let param_def = ts_fn_param_to_param_def(
            param,
            Some(&doc_parser.ast_parser.source_map),
          );
          params.push(param_def);
        }

        let ts_type = ts_prop_sig
          .type_ann
          .as_ref()
          .map(|rt| ts_type_ann_to_def(rt));

        let type_params = maybe_type_param_decl_to_type_param_defs(
          ts_prop_sig.type_params.as_ref(),
        );

        let prop_def = InterfacePropertyDef {
          name,
          js_doc: prop_js_doc,
          location: doc_parser
            .ast_parser
            .get_span_location(ts_prop_sig.span)
            .into(),
          params,
          ts_type,
          computed: ts_prop_sig.computed,
          optional: ts_prop_sig.optional,
          type_params,
        };
        properties.push(prop_def);
      }
      TsCallSignatureDecl(ts_call_sig) => {
        let call_sig_js_doc = doc_parser.js_doc_for_span(ts_call_sig.span);

        let mut params = vec![];
        for param in &ts_call_sig.params {
          let param_def = ts_fn_param_to_param_def(
            param,
            Some(&doc_parser.ast_parser.source_map),
          );
          params.push(param_def);
        }

        let ts_type = ts_call_sig
          .type_ann
          .as_ref()
          .map(|rt| ts_type_ann_to_def(rt));

        let type_params = maybe_type_param_decl_to_type_param_defs(
          ts_call_sig.type_params.as_ref(),
        );

        let call_sig_def = InterfaceCallSignatureDef {
          js_doc: call_sig_js_doc,
          location: doc_parser
            .ast_parser
            .get_span_location(ts_call_sig.span)
            .into(),
          params,
          ts_type,
          type_params,
        };
        call_signatures.push(call_sig_def);
      }
      TsIndexSignature(ts_index_sig) => {
        let mut params = vec![];
        for param in &ts_index_sig.params {
          let param_def = ts_fn_param_to_param_def(param, None);
          params.push(param_def);
        }

        let ts_type = ts_index_sig
          .type_ann
          .as_ref()
          .map(|rt| (&*rt.type_ann).into());

        let index_sig_def = InterfaceIndexSignatureDef {
          readonly: ts_index_sig.readonly,
          params,
          ts_type,
        };
        index_signatures.push(index_sig_def);
      }
      TsConstructSignatureDecl(ts_construct_sig) => {
        let construct_js_doc =
          doc_parser.js_doc_for_span(ts_construct_sig.span);

        let mut params = vec![];

        for param in &ts_construct_sig.params {
          let param_def = ts_fn_param_to_param_def(
            param,
            Some(&doc_parser.ast_parser.source_map),
          );
          params.push(param_def);
        }

        let type_params = maybe_type_param_decl_to_type_param_defs(
          ts_construct_sig.type_params.as_ref(),
        );

        let construct_sig_def = InterfaceMethodDef {
          name: "new".to_string(),
          kind: swc_ecmascript::ast::MethodKind::Method,
          js_doc: construct_js_doc,
          location: doc_parser
            .ast_parser
            .get_span_location(ts_construct_sig.span)
            .into(),
          optional: false,
          params,
          return_type: None,
          type_params,
        };

        methods.push(construct_sig_def);
      }
    }
  }

  let type_params = maybe_type_param_decl_to_type_param_defs(
    interface_decl.type_params.as_ref(),
  );

  let extends = interface_decl
    .extends
    .iter()
    .map(|expr| expr.into())
    .collect::<Vec<TsTypeDef>>();

  let interface_def = InterfaceDef {
    extends,
    methods,
    properties,
    call_signatures,
    index_signatures,
    type_params,
  };

  (interface_name, interface_def)
}

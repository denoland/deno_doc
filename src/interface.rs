// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use deno_ast::ParsedSource;
use deno_ast::SourceRangedForSpanned;
use serde::Deserialize;
use serde::Serialize;

use crate::function::FunctionDef;
use crate::js_doc::JsDoc;
use crate::node::DeclarationKind;
use crate::params::ts_fn_param_to_param_def;
use crate::ts_type::TsTypeDef;
use crate::ts_type_param::maybe_type_param_decl_to_type_param_defs;
use crate::ts_type_param::TsTypeParamDef;
use crate::util::swc::get_location;
use crate::util::swc::is_false;
use crate::util::swc::js_doc_for_range;
use crate::variable::VariableDef;
use crate::DocNode;
use crate::Location;
use crate::ParamDef;

cfg_if! {
  if #[cfg(feature = "rust")] {
    use crate::display::display_computed;
    use crate::display::display_optional;
    use crate::display::display_readonly;
    use crate::display::SliceDisplayer;

    use std::fmt::Display;
    use std::fmt::Formatter;
    use std::fmt::Result as FmtResult;
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct InterfaceMethodDef {
  pub name: String,
  pub kind: deno_ast::swc::ast::MethodKind,
  pub location: Location,
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,
  #[serde(skip_serializing_if = "is_false", default)]
  pub computed: bool,
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
      DeclarationKind::Private,
      def.js_doc,
      FunctionDef {
        def_name: None,
        params: def.params,
        return_type: def.return_type,
        has_body: false,
        is_async: false,
        is_generator: false,
        type_params: def.type_params,
        decorators: vec![],
      },
    )
  }
}

#[cfg(feature = "rust")]
impl Display for InterfaceMethodDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{}{}({})",
      display_computed(self.computed, &self.name),
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
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,
  pub params: Vec<ParamDef>,
  #[serde(skip_serializing_if = "is_false", default)]
  pub readonly: bool,
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
      DeclarationKind::Private,
      def.js_doc,
      VariableDef {
        ts_type: def.ts_type,
        kind: deno_ast::swc::ast::VarDeclKind::Const,
      },
    )
  }
}

#[cfg(feature = "rust")]
impl Display for InterfacePropertyDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{}{}{}",
      display_readonly(self.readonly),
      display_computed(self.computed, &self.name),
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
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,
  pub readonly: bool,
  pub params: Vec<ParamDef>,
  pub ts_type: Option<TsTypeDef>,
  pub location: Location,
}

#[cfg(feature = "rust")]
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
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,
  pub params: Vec<ParamDef>,
  pub ts_type: Option<TsTypeDef>,
  pub type_params: Vec<TsTypeParamDef>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct InterfaceDef {
  #[serde(skip_serializing_if = "Option::is_none", default)]
  /// set when the interface is a default export
  pub def_name: Option<String>,
  pub extends: Vec<TsTypeDef>,
  pub methods: Vec<InterfaceMethodDef>,
  pub properties: Vec<InterfacePropertyDef>,
  pub call_signatures: Vec<InterfaceCallSignatureDef>,
  pub index_signatures: Vec<InterfaceIndexSignatureDef>,
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

          let maybe_return_type =
            ts_method_sig.type_ann.as_deref().map(|type_ann| {
              TsTypeDef::new(parsed_source, &type_ann.type_ann)
            });

          let type_params = maybe_type_param_decl_to_type_param_defs(
            parsed_source,
            ts_method_sig.type_params.as_deref(),
          );

          let method_def = InterfaceMethodDef {
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

          let maybe_return_type =
            ts_getter_sig.type_ann.as_deref().map(|type_ann| {
              TsTypeDef::new(parsed_source, &type_ann.type_ann)
            });

          let method_def = InterfaceMethodDef {
            name,
            kind: deno_ast::swc::ast::MethodKind::Getter,
            js_doc: method_js_doc,
            location: get_location(parsed_source, ts_getter_sig.start()),
            computed: ts_getter_sig.computed,
            optional: ts_getter_sig.optional,
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

          let method_def = InterfaceMethodDef {
            name,
            kind: deno_ast::swc::ast::MethodKind::Setter,
            js_doc: method_js_doc,
            location: get_location(parsed_source, ts_setter_sig.start()),
            computed: ts_setter_sig.computed,
            optional: ts_setter_sig.optional,
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

          let mut params = vec![];

          for param in &ts_prop_sig.params {
            let param_def = ts_fn_param_to_param_def(parsed_source, param);
            params.push(param_def);
          }

          let ts_type = ts_prop_sig.type_ann.as_deref().map(|type_ann| {
            TsTypeDef::new(parsed_source, &type_ann.type_ann)
          });

          let type_params = maybe_type_param_decl_to_type_param_defs(
            parsed_source,
            ts_prop_sig.type_params.as_deref(),
          );

          let prop_def = InterfacePropertyDef {
            name,
            js_doc: prop_js_doc,
            location: get_location(parsed_source, ts_prop_sig.start()),
            params,
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

          let ts_type = ts_call_sig.type_ann.as_deref().map(|type_ann| {
            TsTypeDef::new(parsed_source, &type_ann.type_ann)
          });

          let type_params = maybe_type_param_decl_to_type_param_defs(
            parsed_source,
            ts_call_sig.type_params.as_deref(),
          );

          let call_sig_def = InterfaceCallSignatureDef {
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

          let index_sig_def = InterfaceIndexSignatureDef {
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
        if let Some(construct_js_doc) =
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

          let construct_sig_def = InterfaceMethodDef {
            name: "new".to_string(),
            kind: deno_ast::swc::ast::MethodKind::Method,
            js_doc: construct_js_doc,
            location: get_location(parsed_source, ts_construct_sig.start()),
            computed: false,
            optional: false,
            params,
            return_type: maybe_return_type,
            type_params,
          };

          methods.push(construct_sig_def);
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
    methods,
    properties,
    call_signatures,
    index_signatures,
    type_params,
  };

  (interface_name, interface_def)
}

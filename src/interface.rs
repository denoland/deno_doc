// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use deno_ast::oxc::ast::ast::Expression;
use deno_ast::oxc::ast::ast::TSInterfaceDeclaration;
use deno_ast::oxc::ast::ast::TSSignature;
use deno_graph::symbols::EsModuleInfo;
use serde::Deserialize;
use serde::Serialize;

use crate::ParamDef;
use crate::params::ParamPatternDef;
use crate::params::formal_params_to_param_defs;
use crate::ts_type::CallSignatureDef;
use crate::ts_type::ConstructorDef;
use crate::ts_type::IndexSignatureDef;
use crate::ts_type::MethodDef;
use crate::ts_type::PropertyDef;
use crate::ts_type::TsTypeDef;
use crate::ts_type_param::TsTypeParamDef;
use crate::ts_type_param::maybe_type_param_decl_to_type_param_defs;
use crate::util::swc::get_location;
use crate::util::swc::js_doc_for_range;
use crate::util::types::MethodKind;

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct InterfaceDef {
  #[serde(skip_serializing_if = "Option::is_none", default)]
  /// set when the interface is a default export
  pub def_name: Option<String>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub extends: Vec<TsTypeDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub constructors: Vec<ConstructorDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub methods: Vec<MethodDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub properties: Vec<PropertyDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub call_signatures: Vec<CallSignatureDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub index_signatures: Vec<IndexSignatureDef>,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub type_params: Box<[TsTypeParamDef]>,
}

pub fn expr_to_name(expr: &Expression) -> String {
  match expr {
    Expression::Identifier(ident) => ident.name.to_string(),
    Expression::StaticMemberExpression(member_expr) => {
      let left = expr_to_name(&member_expr.object);
      let right = format!(".{}", member_expr.property.name);
      format!("[{}{}]", left, right)
    }
    Expression::ComputedMemberExpression(_)
    | Expression::PrivateFieldExpression(_) => "[UNSUPPORTED]".to_string(),
    Expression::StringLiteral(str_) => str_.value.to_string(),
    Expression::BooleanLiteral(bool_) => {
      if bool_.value { "true" } else { "false" }.to_string()
    }
    Expression::NullLiteral(_) => "null".to_string(),
    Expression::NumericLiteral(num) => num.value.to_string(),
    Expression::BigIntLiteral(num) => num
      .raw
      .as_ref()
      .map(|r| r.as_str().to_string())
      .unwrap_or_default(),
    Expression::RegExpLiteral(regex) => {
      format!("/{}/", regex.regex.pattern.text)
    }
    Expression::TemplateLiteral(tpl) => {
      let mut result = String::new();
      for (i, quasi) in tpl.quasis.iter().enumerate() {
        result.push_str(quasi.value.raw.as_str());
        if i < tpl.expressions.len() {
          result.push_str(&expr_to_name(&tpl.expressions[i]));
        }
      }
      result
    }
    _ => "[UNSUPPORTED]".to_string(),
  }
}

pub fn property_key_name(key: &deno_ast::oxc::ast::ast::PropertyKey) -> String {
  use deno_ast::oxc::ast::ast::PropertyKey;
  if let Some(name) = key.static_name() {
    return name.to_string();
  }
  // For computed keys, resolve common expression patterns
  match key {
    PropertyKey::StaticIdentifier(ident) => ident.name.to_string(),
    // Computed keys that are identifiers (e.g., `[pc]`)
    PropertyKey::Identifier(ident) => ident.name.to_string(),
    PropertyKey::StringLiteral(s) => s.value.to_string(),
    PropertyKey::NumericLiteral(n) => n.value.to_string(),
    _ => "[UNSUPPORTED]".to_string(),
  }
}

pub fn get_doc_for_ts_interface_decl(
  module_info: &EsModuleInfo,
  interface_decl: &TSInterfaceDeclaration,
  def_name: Option<String>,
) -> InterfaceDef {
  let mut constructors = vec![];
  let mut methods = vec![];
  let mut properties = vec![];
  let mut call_signatures = vec![];
  let mut index_signatures = vec![];

  for type_element in &interface_decl.body.body {
    match type_element {
      TSSignature::TSMethodSignature(ts_method_sig) => {
        if let Some(method_js_doc) =
          js_doc_for_range(module_info, ts_method_sig.span)
        {
          let params =
            formal_params_to_param_defs(module_info, &ts_method_sig.params);

          let name = property_key_name(&ts_method_sig.key);

          let maybe_return_type =
            ts_method_sig.return_type.as_ref().map(|type_ann| {
              TsTypeDef::new(module_info, &type_ann.type_annotation)
            });

          let type_params = maybe_type_param_decl_to_type_param_defs(
            module_info,
            ts_method_sig.type_parameters.as_deref(),
          );

          let method_def = MethodDef {
            name,
            kind: MethodKind::from(ts_method_sig.kind),
            js_doc: method_js_doc,
            location: get_location(module_info, ts_method_sig.span.start),
            computed: ts_method_sig.computed,
            optional: ts_method_sig.optional,
            params,
            return_type: maybe_return_type,
            type_params,
          };
          methods.push(method_def);
        }
      }
      TSSignature::TSPropertySignature(ts_prop_sig) => {
        if let Some(prop_js_doc) =
          js_doc_for_range(module_info, ts_prop_sig.span)
        {
          let name = property_key_name(&ts_prop_sig.key);

          let ts_type = ts_prop_sig.type_annotation.as_ref().map(|type_ann| {
            TsTypeDef::new(module_info, &type_ann.type_annotation)
          });

          let type_params =
            maybe_type_param_decl_to_type_param_defs(module_info, None);

          let prop_def = PropertyDef {
            name,
            js_doc: prop_js_doc,
            location: get_location(module_info, ts_prop_sig.span.start),
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
      TSSignature::TSCallSignatureDeclaration(ts_call_sig) => {
        if let Some(call_sig_js_doc) =
          js_doc_for_range(module_info, ts_call_sig.span)
        {
          let params =
            formal_params_to_param_defs(module_info, &ts_call_sig.params);

          let ts_type = ts_call_sig.return_type.as_ref().map(|type_ann| {
            TsTypeDef::new(module_info, &type_ann.type_annotation)
          });

          let type_params = maybe_type_param_decl_to_type_param_defs(
            module_info,
            ts_call_sig.type_parameters.as_deref(),
          );

          let call_sig_def = CallSignatureDef {
            js_doc: call_sig_js_doc,
            location: get_location(module_info, ts_call_sig.span.start),
            params,
            ts_type,
            type_params,
          };
          call_signatures.push(call_sig_def);
        }
      }
      TSSignature::TSIndexSignature(ts_index_sig) => {
        if let Some(js_doc) = js_doc_for_range(module_info, ts_index_sig.span) {
          // TSIndexSignature uses TSIndexSignatureName, not FormalParameter
          let params = ts_index_sig
            .parameters
            .iter()
            .map(|param| {
              let ts_type = Some(TsTypeDef::new(
                module_info,
                &param.type_annotation.type_annotation,
              ));
              ParamDef {
                pattern: ParamPatternDef::Identifier {
                  name: param.name.to_string(),
                  optional: false,
                },
                decorators: Box::new([]),
                ts_type,
              }
            })
            .collect();

          let ts_type = Some(TsTypeDef::new(
            module_info,
            &ts_index_sig.type_annotation.type_annotation,
          ));

          let index_sig_def = IndexSignatureDef {
            location: get_location(module_info, ts_index_sig.span.start),
            js_doc,
            readonly: ts_index_sig.readonly,
            params,
            ts_type,
          };
          index_signatures.push(index_sig_def);
        }
      }
      TSSignature::TSConstructSignatureDeclaration(ts_construct_sig) => {
        if let Some(js_doc) =
          js_doc_for_range(module_info, ts_construct_sig.span)
        {
          let params =
            formal_params_to_param_defs(module_info, &ts_construct_sig.params);

          let type_params = maybe_type_param_decl_to_type_param_defs(
            module_info,
            ts_construct_sig.type_parameters.as_deref(),
          );

          let maybe_return_type = ts_construct_sig
            .return_type
            .as_ref()
            .map(|rt| TsTypeDef::new(module_info, &rt.type_annotation));

          let construct_sig_def = ConstructorDef {
            js_doc,
            location: get_location(module_info, ts_construct_sig.span.start),
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
    module_info,
    interface_decl.type_parameters.as_deref(),
  );

  let extends = interface_decl
    .extends
    .iter()
    .map(|expr| TsTypeDef::ts_interface_heritage(module_info, expr))
    .collect::<Vec<TsTypeDef>>();

  InterfaceDef {
    def_name,
    extends,
    constructors,
    methods,
    properties,
    call_signatures,
    index_signatures,
    type_params,
  }
}

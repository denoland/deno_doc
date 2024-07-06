// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::colors;
use crate::display::display_computed;
use crate::display::display_optional;
use crate::display::display_readonly;
use crate::display::SliceDisplayer;
use crate::interface::expr_to_name;
use crate::params::param_to_param_def;
use crate::params::pat_to_param_def;
use crate::params::prop_name_to_string;
use crate::params::ts_fn_param_to_param_def;
use crate::ts_type_param::maybe_type_param_decl_to_type_param_defs;
use crate::ts_type_param::TsTypeParamDef;
use crate::util::swc::get_location;
use crate::util::swc::is_false;
use crate::util::swc::js_doc_for_range;
use crate::DocNode;
use crate::Location;
use crate::ParamDef;

use crate::function::FunctionDef;
use crate::js_doc::JsDoc;
use crate::node::DeclarationKind;
use crate::variable::VariableDef;
use deno_ast::swc::ast::*;
use deno_ast::ParsedSource;
use deno_ast::SourceRangedForSpanned;
use serde::Deserialize;
use serde::Serialize;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;

impl TsTypeDef {
  fn ts_lit_type(parsed_source: &ParsedSource, other: &TsLitType) -> Self {
    match &other.lit {
      TsLit::Number(num) => TsTypeDef::number_literal(num),
      TsLit::Str(str_) => TsTypeDef::string_literal(str_),
      TsLit::Tpl(tpl) => TsTypeDef::tpl_literal(
        parsed_source,
        tpl
          .types
          .iter()
          .map(|types| TsTypeDef::new(parsed_source, types))
          .collect::<Vec<_>>(),
        &tpl.quasis,
      ),
      TsLit::Bool(bool_) => TsTypeDef::bool_literal(bool_),
      TsLit::BigInt(bigint_) => TsTypeDef::bigint_literal(bigint_),
    }
  }

  fn ts_array_type(parsed_source: &ParsedSource, other: &TsArrayType) -> Self {
    let ts_type_def = TsTypeDef::new(parsed_source, &other.elem_type);

    TsTypeDef {
      array: Some(Box::new(ts_type_def)),
      kind: Some(TsTypeDefKind::Array),
      ..Default::default()
    }
  }

  fn ts_tuple_type(parsed_source: &ParsedSource, other: &TsTupleType) -> Self {
    let type_defs = other
      .elem_types
      .iter()
      .map(|type_box| TsTypeDef::new(parsed_source, &type_box.ty))
      .collect::<Vec<_>>();

    TsTypeDef {
      tuple: Some(type_defs),
      kind: Some(TsTypeDefKind::Tuple),
      ..Default::default()
    }
  }

  fn ts_union_or_intersection_type(
    parsed_source: &ParsedSource,
    other: &TsUnionOrIntersectionType,
  ) -> Self {
    use deno_ast::swc::ast::TsUnionOrIntersectionType::*;

    match other {
      TsUnionType(union_type) => {
        let types_union = union_type
          .types
          .iter()
          .map(|ts_type| TsTypeDef::new(parsed_source, ts_type))
          .collect::<Vec<_>>();

        TsTypeDef {
          union: Some(types_union),
          kind: Some(TsTypeDefKind::Union),
          ..Default::default()
        }
      }
      TsIntersectionType(intersection_type) => {
        let types_intersection = intersection_type
          .types
          .iter()
          .map(|ts_type| TsTypeDef::new(parsed_source, ts_type))
          .collect::<Vec<_>>();

        TsTypeDef {
          intersection: Some(types_intersection),
          kind: Some(TsTypeDefKind::Intersection),
          ..Default::default()
        }
      }
    }
  }

  fn ts_keyword_type(
    _parsed_source: &ParsedSource,
    other: &TsKeywordType,
  ) -> Self {
    use deno_ast::swc::ast::TsKeywordTypeKind::*;

    let keyword_str = match other.kind {
      TsAnyKeyword => "any",
      TsUnknownKeyword => "unknown",
      TsNumberKeyword => "number",
      TsObjectKeyword => "object",
      TsBooleanKeyword => "boolean",
      TsBigIntKeyword => "bigint",
      TsStringKeyword => "string",
      TsSymbolKeyword => "symbol",
      TsVoidKeyword => "void",
      TsUndefinedKeyword => "undefined",
      TsNullKeyword => "null",
      TsNeverKeyword => "never",
      TsIntrinsicKeyword => "intrinsic",
    };

    TsTypeDef::keyword(keyword_str)
  }

  fn ts_type_operator(
    parsed_source: &ParsedSource,
    other: &TsTypeOperator,
  ) -> Self {
    let ts_type = TsTypeDef::new(parsed_source, &other.type_ann);
    let type_operator_def = TsTypeOperatorDef {
      operator: other.op.as_str().to_string(),
      ts_type,
    };

    TsTypeDef {
      type_operator: Some(Box::new(type_operator_def)),
      kind: Some(TsTypeDefKind::TypeOperator),
      ..Default::default()
    }
  }

  fn ts_parenthesized_type(
    parsed_source: &ParsedSource,
    other: &TsParenthesizedType,
  ) -> Self {
    let ts_type = TsTypeDef::new(parsed_source, &other.type_ann);

    TsTypeDef {
      parenthesized: Some(Box::new(ts_type)),
      kind: Some(TsTypeDefKind::Parenthesized),
      ..Default::default()
    }
  }

  fn ts_rest_type(parsed_source: &ParsedSource, other: &TsRestType) -> Self {
    let ts_type = TsTypeDef::new(parsed_source, &other.type_ann);

    TsTypeDef {
      rest: Some(Box::new(ts_type)),
      kind: Some(TsTypeDefKind::Rest),
      ..Default::default()
    }
  }

  fn ts_optional_type(
    parsed_source: &ParsedSource,
    other: &TsOptionalType,
  ) -> Self {
    let ts_type = TsTypeDef::new(parsed_source, &other.type_ann);

    TsTypeDef {
      optional: Some(Box::new(ts_type)),
      kind: Some(TsTypeDefKind::Optional),
      ..Default::default()
    }
  }

  fn ts_this_type(_parsed_source: &ParsedSource, _other: &TsThisType) -> Self {
    TsTypeDef {
      repr: "this".to_string(),
      this: Some(true),
      kind: Some(TsTypeDefKind::This),
      ..Default::default()
    }
  }

  fn ts_type_predicate(
    parsed_source: &ParsedSource,
    other: &TsTypePredicate,
  ) -> Self {
    let pred = TsTypePredicateDef {
      asserts: other.asserts,
      param: (&other.param_name).into(),
      r#type: other
        .type_ann
        .as_ref()
        .map(|t| Box::new(TsTypeDef::new(parsed_source, &t.type_ann))),
    };
    TsTypeDef {
      repr: pred.to_string(),
      kind: Some(TsTypeDefKind::TypePredicate),
      type_predicate: Some(pred),
      ..Default::default()
    }
  }

  fn ts_type_query(_parsed_source: &ParsedSource, other: &TsTypeQuery) -> Self {
    use deno_ast::swc::ast::TsTypeQueryExpr::*;

    let type_name = match &other.expr_name {
      TsEntityName(entity_name) => ts_entity_name_to_name(entity_name),
      Import(import_type) => import_type.arg.value.to_string(),
    };

    TsTypeDef {
      repr: type_name.to_string(),
      type_query: Some(type_name),
      kind: Some(TsTypeDefKind::TypeQuery),
      ..Default::default()
    }
  }

  fn ts_type_ref(parsed_source: &ParsedSource, other: &TsTypeRef) -> Self {
    let type_name = ts_entity_name_to_name(&other.type_name);

    let type_params = if let Some(type_params_inst) = &other.type_params {
      let ts_type_defs = type_params_inst
        .params
        .iter()
        .map(|ts_type| TsTypeDef::new(parsed_source, ts_type))
        .collect::<Vec<_>>();

      Some(ts_type_defs)
    } else {
      None
    };

    TsTypeDef {
      repr: type_name.clone(),
      type_ref: Some(TsTypeRefDef {
        type_params,
        type_name,
      }),
      kind: Some(TsTypeDefKind::TypeRef),
      ..Default::default()
    }
  }

  pub fn ts_expr_with_type_args(
    parsed_source: &ParsedSource,
    other: &TsExprWithTypeArgs,
  ) -> Self {
    let type_name = expr_to_name(&other.expr);

    let type_params = if let Some(type_params_inst) = &other.type_args {
      let ts_type_defs = type_params_inst
        .params
        .iter()
        .map(|ts_type| TsTypeDef::new(parsed_source, ts_type))
        .collect::<Vec<_>>();

      Some(ts_type_defs)
    } else {
      None
    };

    TsTypeDef {
      repr: type_name.clone(),
      type_ref: Some(TsTypeRefDef {
        type_params,
        type_name,
      }),
      kind: Some(TsTypeDefKind::TypeRef),
      ..Default::default()
    }
  }

  fn ts_indexed_access_type(
    parsed_source: &ParsedSource,
    other: &TsIndexedAccessType,
  ) -> Self {
    TsTypeDef::new(parsed_source, &other.obj_type);
    let indexed_access_def = TsIndexedAccessDef {
      readonly: other.readonly,
      obj_type: Box::new(TsTypeDef::new(parsed_source, &other.obj_type)),
      index_type: Box::new(TsTypeDef::new(parsed_source, &other.index_type)),
    };

    TsTypeDef {
      indexed_access: Some(indexed_access_def),
      kind: Some(TsTypeDefKind::IndexedAccess),
      ..Default::default()
    }
  }

  fn ts_mapped_type(
    parsed_source: &ParsedSource,
    other: &TsMappedType,
  ) -> Self {
    let mapped_type_def = TsMappedTypeDef {
      readonly: other.readonly,
      type_param: Box::new(TsTypeParamDef::new(
        parsed_source,
        &other.type_param,
      )),
      name_type: other
        .name_type
        .as_ref()
        .map(|nt| Box::new(TsTypeDef::new(parsed_source, nt))),
      optional: other.optional,
      ts_type: other
        .type_ann
        .as_ref()
        .map(|a| Box::new(TsTypeDef::new(parsed_source, a))),
    };

    TsTypeDef {
      mapped_type: Some(mapped_type_def),
      kind: Some(TsTypeDefKind::Mapped),
      ..Default::default()
    }
  }

  fn ts_type_lit(parsed_source: &ParsedSource, other: &TsTypeLit) -> Self {
    let mut constructors = vec![];
    let mut methods = vec![];
    let mut properties = vec![];
    let mut call_signatures = vec![];
    let mut index_signatures = vec![];

    for type_element in &other.members {
      use deno_ast::swc::ast::TsTypeElement::*;

      match &type_element {
        TsMethodSignature(ts_method_sig) => {
          if let Some(js_doc) =
            js_doc_for_range(parsed_source, &ts_method_sig.range())
          {
            let params = ts_method_sig
              .params
              .iter()
              .map(|param| ts_fn_param_to_param_def(parsed_source, param))
              .collect::<Vec<_>>();

            let maybe_return_type = ts_method_sig
              .type_ann
              .as_ref()
              .map(|rt| TsTypeDef::new(parsed_source, &rt.type_ann));

            let type_params = maybe_type_param_decl_to_type_param_defs(
              parsed_source,
              ts_method_sig.type_params.as_deref(),
            );
            let name = expr_to_name(&ts_method_sig.key);
            let method_def = MethodDef {
              name,
              js_doc,
              kind: MethodKind::Method,
              location: get_location(parsed_source, ts_method_sig.start()),
              params,
              computed: ts_method_sig.computed,
              optional: ts_method_sig.optional,
              return_type: maybe_return_type,
              type_params,
            };
            methods.push(method_def);
          }
        }
        TsGetterSignature(ts_getter_sig) => {
          if let Some(js_doc) =
            js_doc_for_range(parsed_source, &ts_getter_sig.range())
          {
            let maybe_return_type = ts_getter_sig
              .type_ann
              .as_ref()
              .map(|rt| TsTypeDef::new(parsed_source, &rt.type_ann));

            let name = expr_to_name(&ts_getter_sig.key);
            let method_def = MethodDef {
              name,
              js_doc,
              kind: MethodKind::Getter,
              location: get_location(parsed_source, ts_getter_sig.start()),
              params: vec![],
              computed: ts_getter_sig.computed,
              optional: ts_getter_sig.optional,
              return_type: maybe_return_type,
              type_params: vec![],
            };
            methods.push(method_def);
          }
        }
        TsSetterSignature(ts_setter_sig) => {
          if let Some(js_doc) =
            js_doc_for_range(parsed_source, &ts_setter_sig.range())
          {
            let name = expr_to_name(&ts_setter_sig.key);

            let params = vec![ts_fn_param_to_param_def(
              parsed_source,
              &ts_setter_sig.param,
            )];

            let method_def = MethodDef {
              name,
              js_doc,
              kind: MethodKind::Setter,
              location: get_location(parsed_source, ts_setter_sig.start()),
              params,
              computed: ts_setter_sig.computed,
              optional: ts_setter_sig.optional,
              return_type: None,
              type_params: vec![],
            };
            methods.push(method_def);
          }
        }
        TsPropertySignature(ts_prop_sig) => {
          if let Some(js_doc) =
            js_doc_for_range(parsed_source, &ts_prop_sig.range())
          {
            let name = expr_to_name(&ts_prop_sig.key);

            let params = ts_prop_sig
              .params
              .iter()
              .map(|param| ts_fn_param_to_param_def(parsed_source, param))
              .collect();

            let ts_type = ts_prop_sig
              .type_ann
              .as_ref()
              .map(|rt| TsTypeDef::new(parsed_source, &rt.type_ann));

            let type_params = maybe_type_param_decl_to_type_param_defs(
              parsed_source,
              ts_prop_sig.type_params.as_deref(),
            );
            let prop_def = PropertyDef {
              name,
              js_doc,
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
          if let Some(js_doc) =
            js_doc_for_range(parsed_source, &ts_call_sig.range())
          {
            let params = ts_call_sig
              .params
              .iter()
              .map(|param| ts_fn_param_to_param_def(parsed_source, param))
              .collect();

            let ts_type = ts_call_sig
              .type_ann
              .as_ref()
              .map(|rt| TsTypeDef::new(parsed_source, &rt.type_ann));

            let type_params = maybe_type_param_decl_to_type_param_defs(
              parsed_source,
              ts_call_sig.type_params.as_deref(),
            );

            let call_sig_def = CallSignatureDef {
              js_doc,
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
            let params = ts_index_sig
              .params
              .iter()
              .map(|param| ts_fn_param_to_param_def(parsed_source, param))
              .collect();

            let ts_type = ts_index_sig
              .type_ann
              .as_ref()
              .map(|rt| TsTypeDef::new(parsed_source, &rt.type_ann));

            let index_sig_def = IndexSignatureDef {
              js_doc,
              location: get_location(parsed_source, ts_index_sig.start()),
              readonly: ts_index_sig.readonly,
              params,
              ts_type,
            };
            index_signatures.push(index_sig_def);
          }
        }
        TsConstructSignatureDecl(ts_construct_sig) => {
          if let Some(prop_js_doc) =
            js_doc_for_range(parsed_source, &ts_construct_sig.range())
          {
            let params = ts_construct_sig
              .params
              .iter()
              .map(|param| ts_fn_param_to_param_def(parsed_source, param))
              .collect();

            let type_params = maybe_type_param_decl_to_type_param_defs(
              parsed_source,
              ts_construct_sig.type_params.as_deref(),
            );

            let maybe_return_type = ts_construct_sig
              .type_ann
              .as_ref()
              .map(|rt| TsTypeDef::new(parsed_source, &rt.type_ann));

            let construct_sig_def = ConstructorDef {
              js_doc: prop_js_doc,
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

    let type_literal = TsTypeLiteralDef {
      constructors,
      methods,
      properties,
      call_signatures,
      index_signatures,
    };

    TsTypeDef {
      kind: Some(TsTypeDefKind::TypeLiteral),
      type_literal: Some(type_literal),
      ..Default::default()
    }
  }

  fn ts_conditional_type(
    parsed_source: &ParsedSource,
    other: &TsConditionalType,
  ) -> Self {
    let conditional_type_def = TsConditionalDef {
      check_type: Box::new(TsTypeDef::new(parsed_source, &other.check_type)),
      extends_type: Box::new(TsTypeDef::new(
        parsed_source,
        &other.extends_type,
      )),
      true_type: Box::new(TsTypeDef::new(parsed_source, &other.true_type)),
      false_type: Box::new(TsTypeDef::new(parsed_source, &other.false_type)),
    };

    TsTypeDef {
      kind: Some(TsTypeDefKind::Conditional),
      conditional_type: Some(conditional_type_def),
      ..Default::default()
    }
  }

  fn ts_infer_type(parsed_source: &ParsedSource, other: &TsInferType) -> Self {
    let infer = TsInferDef {
      type_param: Box::new(TsTypeParamDef::new(
        parsed_source,
        &other.type_param,
      )),
    };

    TsTypeDef {
      kind: Some(TsTypeDefKind::Infer),
      infer: Some(infer),
      ..Default::default()
    }
  }

  fn ts_import_type(
    parsed_source: &ParsedSource,
    other: &TsImportType,
  ) -> Self {
    let type_params = if let Some(type_params_inst) = &other.type_args {
      let ts_type_defs = type_params_inst
        .params
        .iter()
        .map(|param| TsTypeDef::new(parsed_source, param))
        .collect::<Vec<_>>();

      Some(ts_type_defs)
    } else {
      None
    };

    let import_type_def = TsImportTypeDef {
      specifier: other.arg.value.to_string(),
      qualifier: other.qualifier.as_ref().map(ts_entity_name_to_name),
      type_params,
    };

    TsTypeDef {
      kind: Some(TsTypeDefKind::ImportType),
      import_type: Some(import_type_def),
      ..Default::default()
    }
  }

  fn ts_fn_or_constructor_type(
    parsed_source: &ParsedSource,
    other: &TsFnOrConstructorType,
  ) -> Self {
    use deno_ast::swc::ast::TsFnOrConstructorType::*;

    let fn_def = match other {
      TsFnType(ts_fn_type) => {
        let params = ts_fn_type
          .params
          .iter()
          .map(|param| ts_fn_param_to_param_def(parsed_source, param))
          .collect();

        let type_params = maybe_type_param_decl_to_type_param_defs(
          parsed_source,
          ts_fn_type.type_params.as_deref(),
        );

        TsFnOrConstructorDef {
          constructor: false,
          ts_type: TsTypeDef::new(parsed_source, &ts_fn_type.type_ann.type_ann),
          params,
          type_params,
        }
      }
      TsConstructorType(ctor_type) => {
        let params = ctor_type
          .params
          .iter()
          .map(|param| ts_fn_param_to_param_def(parsed_source, param))
          .collect();

        let type_params = maybe_type_param_decl_to_type_param_defs(
          parsed_source,
          ctor_type.type_params.as_deref(),
        );
        TsFnOrConstructorDef {
          constructor: true,
          ts_type: TsTypeDef::new(parsed_source, &ctor_type.type_ann.type_ann),
          params,
          type_params,
        }
      }
    };

    TsTypeDef {
      kind: Some(TsTypeDefKind::FnOrConstructor),
      fn_or_constructor: Some(Box::new(fn_def)),
      ..Default::default()
    }
  }

  pub fn new(parsed_source: &ParsedSource, other: &TsType) -> Self {
    use deno_ast::swc::ast::TsType::*;

    match other {
      TsKeywordType(keyword_type) => {
        TsTypeDef::ts_keyword_type(parsed_source, keyword_type)
      }
      TsThisType(this_type) => {
        TsTypeDef::ts_this_type(parsed_source, this_type)
      }
      TsFnOrConstructorType(fn_or_con_type) => {
        TsTypeDef::ts_fn_or_constructor_type(parsed_source, fn_or_con_type)
      }
      TsTypeRef(type_ref) => TsTypeDef::ts_type_ref(parsed_source, type_ref),
      TsTypeQuery(type_query) => {
        TsTypeDef::ts_type_query(parsed_source, type_query)
      }
      TsTypeLit(type_literal) => {
        TsTypeDef::ts_type_lit(parsed_source, type_literal)
      }
      TsArrayType(array_type) => {
        TsTypeDef::ts_array_type(parsed_source, array_type)
      }
      TsTupleType(tuple_type) => {
        TsTypeDef::ts_tuple_type(parsed_source, tuple_type)
      }
      TsOptionalType(optional_type) => {
        TsTypeDef::ts_optional_type(parsed_source, optional_type)
      }
      TsRestType(rest_type) => {
        TsTypeDef::ts_rest_type(parsed_source, rest_type)
      }
      TsUnionOrIntersectionType(union_or_inter) => {
        TsTypeDef::ts_union_or_intersection_type(parsed_source, union_or_inter)
      }
      TsConditionalType(conditional_type) => {
        TsTypeDef::ts_conditional_type(parsed_source, conditional_type)
      }
      TsInferType(infer_type) => {
        TsTypeDef::ts_infer_type(parsed_source, infer_type)
      }
      TsParenthesizedType(paren_type) => {
        TsTypeDef::ts_parenthesized_type(parsed_source, paren_type)
      }
      TsTypeOperator(type_op_type) => {
        TsTypeDef::ts_type_operator(parsed_source, type_op_type)
      }
      TsIndexedAccessType(indexed_access_type) => {
        TsTypeDef::ts_indexed_access_type(parsed_source, indexed_access_type)
      }
      TsMappedType(mapped_type) => {
        TsTypeDef::ts_mapped_type(parsed_source, mapped_type)
      }
      TsLitType(lit_type) => TsTypeDef::ts_lit_type(parsed_source, lit_type),
      TsTypePredicate(type_predicate_type) => {
        TsTypeDef::ts_type_predicate(parsed_source, type_predicate_type)
      }
      TsImportType(import_type) => {
        TsTypeDef::ts_import_type(parsed_source, import_type)
      }
    }
  }
}

fn ts_entity_name_to_name(entity_name: &TsEntityName) -> String {
  use deno_ast::swc::ast::TsEntityName::*;

  match entity_name {
    Ident(ident) => ident.sym.to_string(),
    TsQualifiedName(ts_qualified_name) => {
      let left = ts_entity_name_to_name(&ts_qualified_name.left);
      let right = ts_qualified_name.right.sym.to_string();
      format!("{}.{}", left, right)
    }
  }
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct TsTypeRefDef {
  pub type_params: Option<Vec<TsTypeDef>>,
  pub type_name: String,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum LiteralDefKind {
  Number,
  String,
  Template,
  Boolean,
  BigInt,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct LiteralDef {
  pub kind: LiteralDefKind,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub number: Option<f64>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub string: Option<String>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub ts_types: Option<Vec<TsTypeDef>>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub boolean: Option<bool>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct TsTypeOperatorDef {
  pub operator: String,
  pub ts_type: TsTypeDef,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct TsFnOrConstructorDef {
  pub constructor: bool,
  pub ts_type: TsTypeDef,
  pub params: Vec<ParamDef>,
  pub type_params: Vec<TsTypeParamDef>,
}

impl TsFnOrConstructorDef {
  fn arrow_expr(parsed_source: &ParsedSource, expr: &ArrowExpr) -> Self {
    let params = expr
      .params
      .iter()
      .map(|pat| pat_to_param_def(parsed_source, pat))
      .collect();
    let ts_type = expr
      .return_type
      .as_deref()
      .map(|return_type| TsTypeDef::new(parsed_source, &return_type.type_ann))
      .unwrap_or_else(|| TsTypeDef::keyword("unknown"));
    let type_params = maybe_type_param_decl_to_type_param_defs(
      parsed_source,
      expr.type_params.as_deref(),
    );

    Self {
      constructor: false,
      ts_type,
      params,
      type_params,
    }
  }

  fn fn_expr(parsed_source: &ParsedSource, expr: &FnExpr) -> Self {
    let params = expr
      .function
      .params
      .iter()
      .map(|param| pat_to_param_def(parsed_source, &param.pat))
      .collect();
    let ts_type = expr
      .function
      .return_type
      .as_deref()
      .map(|return_type| TsTypeDef::new(parsed_source, &return_type.type_ann))
      .unwrap_or_else(|| TsTypeDef::keyword("unknown"));
    let type_params = maybe_type_param_decl_to_type_param_defs(
      parsed_source,
      expr.function.type_params.as_deref(),
    );

    Self {
      constructor: false,
      ts_type,
      params,
      type_params,
    }
  }
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct TsConditionalDef {
  pub check_type: Box<TsTypeDef>,
  pub extends_type: Box<TsTypeDef>,
  pub true_type: Box<TsTypeDef>,
  pub false_type: Box<TsTypeDef>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct TsInferDef {
  pub type_param: Box<TsTypeParamDef>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct TsImportTypeDef {
  pub specifier: String,
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub qualifier: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub type_params: Option<Vec<TsTypeDef>>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct TsIndexedAccessDef {
  pub readonly: bool,
  pub obj_type: Box<TsTypeDef>,
  pub index_type: Box<TsTypeDef>,
}

/// Mapped Types
///
/// ```ts
/// readonly [Properties in keyof Type as NewType]: Type[Properties]
/// ```
///
/// - `readonly` = `TruePlusMinus::True`
/// - `type_param` = `Some(TsTypeParamDef)` (`Properties in keyof Type`)
/// - `name_type` = `Some(TsTypeDef)` (`NewType`)
/// - `optional` = `None`
/// - `ts_type` = `Some(TsTypeDef)` (`Type[Properties]`)
///
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct TsMappedTypeDef {
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub readonly: Option<TruePlusMinus>,
  pub type_param: Box<TsTypeParamDef>,
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub name_type: Option<Box<TsTypeDef>>,
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub optional: Option<TruePlusMinus>,
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub ts_type: Option<Box<TsTypeDef>>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct ConstructorDef {
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,
  pub params: Vec<ParamDef>,
  pub return_type: Option<TsTypeDef>,
  pub type_params: Vec<TsTypeParamDef>,
  pub location: Location,
}

impl Display for ConstructorDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{}({})",
      colors::magenta("constructor"),
      SliceDisplayer::new(&self.params, ", ", false),
    )
  }
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct MethodDef {
  pub name: String,
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,
  pub kind: MethodKind,
  #[serde(default)]
  pub location: Location,
  pub params: Vec<ParamDef>,
  #[serde(skip_serializing_if = "is_false", default)]
  pub computed: bool,
  pub optional: bool,
  pub return_type: Option<TsTypeDef>,
  pub type_params: Vec<TsTypeParamDef>,
}

impl From<MethodDef> for DocNode {
  fn from(def: MethodDef) -> DocNode {
    DocNode::function(
      def.name,
      false,
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

impl Display for MethodDef {
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

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct PropertyDef {
  pub name: String,
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,
  #[serde(default)]
  pub location: Location,
  pub params: Vec<ParamDef>,
  #[serde(skip_serializing_if = "is_false", default)]
  pub readonly: bool,
  pub computed: bool,
  pub optional: bool,
  pub ts_type: Option<TsTypeDef>,
  pub type_params: Vec<TsTypeParamDef>,
}

impl From<PropertyDef> for DocNode {
  fn from(def: PropertyDef) -> DocNode {
    DocNode::variable(
      def.name,
      false,
      def.location,
      DeclarationKind::Private,
      def.js_doc,
      VariableDef {
        ts_type: def.ts_type,
        kind: VarDeclKind::Const,
      },
    )
  }
}

impl Display for PropertyDef {
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

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct CallSignatureDef {
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,
  #[serde(default)]
  pub location: Location,
  pub params: Vec<ParamDef>,
  pub ts_type: Option<TsTypeDef>,
  pub type_params: Vec<TsTypeParamDef>,
}

impl Display for CallSignatureDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "({})", SliceDisplayer::new(&self.params, ", ", false))?;
    if let Some(ts_type) = &self.ts_type {
      write!(f, ": {}", ts_type)?;
    }
    Ok(())
  }
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct IndexSignatureDef {
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,
  pub readonly: bool,
  pub params: Vec<ParamDef>,
  pub ts_type: Option<TsTypeDef>,
  #[serde(default)]
  pub location: Location,
}

impl Display for IndexSignatureDef {
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

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct TsTypeLiteralDef {
  #[serde(default)]
  pub constructors: Vec<ConstructorDef>,
  pub methods: Vec<MethodDef>,
  pub properties: Vec<PropertyDef>,
  pub call_signatures: Vec<CallSignatureDef>,
  pub index_signatures: Vec<IndexSignatureDef>,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub enum TsTypeDefKind {
  Keyword,
  Literal,
  TypeRef,
  Union,
  Intersection,
  Array,
  Tuple,
  TypeOperator,
  Parenthesized,
  Rest,
  Optional,
  TypeQuery,
  This,
  FnOrConstructor,
  Conditional,
  Infer,
  IndexedAccess,
  Mapped,
  TypeLiteral,
  TypePredicate,
  ImportType,
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct TsTypeDef {
  pub repr: String,

  pub kind: Option<TsTypeDefKind>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub keyword: Option<String>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub literal: Option<LiteralDef>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub type_ref: Option<TsTypeRefDef>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub union: Option<Vec<TsTypeDef>>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub intersection: Option<Vec<TsTypeDef>>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub array: Option<Box<TsTypeDef>>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub tuple: Option<Vec<TsTypeDef>>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub type_operator: Option<Box<TsTypeOperatorDef>>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub parenthesized: Option<Box<TsTypeDef>>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub rest: Option<Box<TsTypeDef>>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub optional: Option<Box<TsTypeDef>>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub type_query: Option<String>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub this: Option<bool>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub fn_or_constructor: Option<Box<TsFnOrConstructorDef>>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub conditional_type: Option<TsConditionalDef>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub infer: Option<TsInferDef>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub indexed_access: Option<TsIndexedAccessDef>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub mapped_type: Option<TsMappedTypeDef>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub type_literal: Option<TsTypeLiteralDef>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub type_predicate: Option<TsTypePredicateDef>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub import_type: Option<TsImportTypeDef>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum ThisOrIdent {
  This,
  Identifier { name: String },
}

impl From<&TsThisTypeOrIdent> for ThisOrIdent {
  fn from(other: &TsThisTypeOrIdent) -> ThisOrIdent {
    use TsThisTypeOrIdent::*;
    match other {
      TsThisType(_) => Self::This,
      Ident(ident) => Self::Identifier {
        name: ident.sym.to_string(),
      },
    }
  }
}

/// ```ts
/// function foo(param: any): asserts param is SomeType { ... }
///                           ^^^^^^^ ^^^^^    ^^^^^^^^
///                           (1)     (2)      (3)
/// ```
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct TsTypePredicateDef {
  /// (1) Whether the predicate includes `asserts` keyword or not
  pub asserts: bool,

  /// (2) The term of predicate
  pub param: ThisOrIdent,

  /// (3) The type against which the parameter is checked
  pub r#type: Option<Box<TsTypeDef>>,
}

impl Display for TsTypePredicateDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    let mut s = Vec::new();
    if self.asserts {
      s.push("asserts".to_string());
    }
    s.push(match &self.param {
      ThisOrIdent::This => "this".to_string(),
      ThisOrIdent::Identifier { name } => name.clone(),
    });
    if let Some(ty) = &self.r#type {
      s.push("is".to_string());
      s.push(ty.to_string());
    }
    write!(f, "{}", s.join(" "))
  }
}

impl TsTypeDef {
  pub fn number_literal(num: &Number) -> Self {
    Self::number_value(num.value)
  }

  pub fn number_value(value: f64) -> Self {
    let repr = format!("{}", value);
    let lit = LiteralDef {
      kind: LiteralDefKind::Number,
      number: Some(value),
      string: None,
      ts_types: None,
      boolean: None,
    };
    Self::literal(repr, lit)
  }

  pub fn string_literal(str_node: &Str) -> Self {
    Self::string_value(str_node.value.to_string())
  }

  pub fn string_value(value: String) -> Self {
    let repr = value.clone();
    let lit = LiteralDef {
      kind: LiteralDefKind::String,
      number: None,
      string: Some(value),
      ts_types: None,
      boolean: None,
    };
    Self::literal(repr, lit)
  }

  pub fn tpl_literal(
    _parsed_source: &ParsedSource,
    types: Vec<TsTypeDef>,
    quasis: &[TplElement],
  ) -> Self {
    let mut types_out: Vec<(Self, String)> = Vec::new();
    for ts_type in types {
      let repr = format!("${{{}}}", ts_type);
      types_out.push((ts_type, repr))
    }
    let mut qasis_out: Vec<(Self, String)> = Vec::new();
    for quasi in quasis {
      let repr = quasi.raw.to_string();
      let lit = LiteralDef {
        kind: LiteralDefKind::String,
        number: None,
        string: Some(repr.clone()),
        ts_types: None,
        boolean: None,
      };
      qasis_out.push((Self::literal(repr.clone(), lit), repr));
    }

    let mut out = vec![];

    let mut types_iter = types_out.into_iter();

    for quasi in qasis_out {
      out.push(quasi);
      if let Some(ts_type) = types_iter.next() {
        out.push(ts_type);
      }
    }

    let repr = out.iter().map(|(_, s)| s.as_str()).collect::<String>();
    let ts_types = Some(out.into_iter().map(|(t, _)| t).collect());
    let lit = LiteralDef {
      kind: LiteralDefKind::Template,
      number: None,
      string: None,
      ts_types,
      boolean: None,
    };
    Self::literal(repr, lit)
  }

  pub fn bool_literal(bool_node: &Bool) -> Self {
    Self::bool_value(bool_node.value)
  }

  pub fn bool_value(value: bool) -> Self {
    let repr = value.to_string();
    let lit = LiteralDef {
      kind: LiteralDefKind::Boolean,
      number: None,
      string: None,
      ts_types: None,
      boolean: Some(value),
    };
    Self::literal(repr, lit)
  }

  pub fn bigint_literal(bigint_node: &BigInt) -> Self {
    let repr = bigint_node.value.to_string();
    let lit = LiteralDef {
      kind: LiteralDefKind::BigInt,
      number: None,
      string: Some(bigint_node.value.to_string()),
      ts_types: None,
      boolean: None,
    };
    Self::literal(repr, lit)
  }

  pub fn regexp(repr: String) -> Self {
    Self {
      repr,
      kind: Some(TsTypeDefKind::TypeRef),
      type_ref: Some(TsTypeRefDef {
        type_params: None,
        type_name: "RegExp".to_string(),
      }),
      ..Default::default()
    }
  }

  pub fn object(methods: Vec<MethodDef>, properties: Vec<PropertyDef>) -> Self {
    Self {
      repr: "".to_string(),
      kind: Some(TsTypeDefKind::TypeLiteral),
      type_literal: Some(TsTypeLiteralDef {
        methods,
        properties,
        ..Default::default()
      }),
      ..Default::default()
    }
  }

  pub fn keyword(keyword_str: &str) -> Self {
    Self::keyword_with_repr(keyword_str, keyword_str)
  }

  pub fn number_with_repr(repr: &str) -> Self {
    Self::keyword_with_repr("number", repr)
  }

  pub fn string_with_repr(repr: &str) -> Self {
    Self::keyword_with_repr("string", repr)
  }

  pub fn bool_with_repr(repr: &str) -> Self {
    Self::keyword_with_repr("boolean", repr)
  }

  pub fn bigint_with_repr(repr: &str) -> Self {
    Self::keyword_with_repr("bigint", repr)
  }

  pub fn keyword_with_repr(keyword_str: &str, repr: &str) -> Self {
    Self {
      repr: repr.to_string(),
      kind: Some(TsTypeDefKind::Keyword),
      keyword: Some(keyword_str.to_string()),
      ..Default::default()
    }
  }

  fn literal(repr: String, lit: LiteralDef) -> Self {
    Self {
      repr,
      kind: Some(TsTypeDefKind::Literal),
      literal: Some(lit),
      ..Default::default()
    }
  }
}

pub fn infer_ts_type_from_expr(
  parsed_source: &ParsedSource,
  expr: &Expr,
  is_const: bool,
) -> Option<TsTypeDef> {
  match expr {
    Expr::Array(arr_lit) => {
      // e.g.) const n = ["a", 1];
      infer_ts_type_from_arr_lit(parsed_source, arr_lit, false)
    }
    Expr::Arrow(expr) => {
      // e.g.) const f = (a: string): void => {};
      infer_ts_type_from_arrow_expr(parsed_source, expr)
    }
    Expr::Fn(expr) => {
      // e.g.) const f = function a(a:string): void {};
      infer_ts_type_from_fn_expr(parsed_source, expr)
    }
    Expr::Lit(lit) => {
      // e.g.) const n = 100;
      infer_ts_type_from_lit(lit, is_const)
    }
    Expr::New(expr) => {
      // e.g.) const d = new Date()
      infer_ts_type_from_new_expr(parsed_source, expr)
    }
    Expr::Tpl(tpl) => {
      // e.g.) const s = `hello`;
      Some(infer_ts_type_from_tpl(parsed_source, tpl, is_const))
    }
    Expr::TsConstAssertion(assertion) => {
      // e.g.) const s = [] as const;
      infer_ts_type_from_const_assertion(parsed_source, assertion)
    }
    Expr::Call(expr) => {
      // e.g.) const value = Number(123);
      infer_ts_type_from_call_expr(expr)
    }
    Expr::Object(obj) => {
      // e.g.) const value = {foo: "bar"};
      infer_ts_type_from_obj(parsed_source, obj)
    }
    Expr::TsSatisfies(satisfies) => {
      // e.g.) const value = {foo: "bar"} satifies Record<string, string>;
      infer_ts_type_from_expr(parsed_source, &satisfies.expr, is_const)
    }
    Expr::Update(_) => {
      // e.g.) let foo = 0;
      //       const bar = foo++;
      Some(TsTypeDef::number_with_repr("number"))
    }
    Expr::TsTypeAssertion(assertion) => {
      // e.g.) export const foo = <string> 1;
      Some(TsTypeDef::new(parsed_source, &assertion.type_ann))
    }
    Expr::TsAs(as_expr) => {
      // e.g.) export const foo = 1 as string;
      Some(TsTypeDef::new(parsed_source, &as_expr.type_ann))
    }
    Expr::Paren(paren) => {
      // e.g.) export const foo = (1);
      infer_ts_type_from_expr(parsed_source, &paren.expr, is_const)
    }
    Expr::Await(await_expr) => {
      // e.g.) export const foo = await 1;
      infer_ts_type_from_expr(parsed_source, &await_expr.arg, is_const)
    }
    Expr::Cond(cond) => {
      // e.g.) export const foo = true ? "a" : 1;
      let left = infer_ts_type_from_expr(parsed_source, &cond.cons, is_const)?;
      let right = infer_ts_type_from_expr(parsed_source, &cond.alt, is_const)?;

      Some(TsTypeDef {
        union: Some(vec![left, right]),
        kind: Some(TsTypeDefKind::Union),
        ..Default::default()
      })
    }
    Expr::TsNonNull(non_null) => {
      // e.g.) export const foo = (true ? "a" : null)!;
      // e.g.) export const foo = null!;
      let with_null =
        infer_ts_type_from_expr(parsed_source, &non_null.expr, is_const)?;

      if let Some(union) = with_null.union {
        let mut non_null_union = union
          .into_iter()
          .filter(|item| {
            if let Some(keyword) = &item.keyword {
              return keyword != "null";
            }

            true
          })
          .collect::<Vec<_>>();

        Some(match non_null_union.len() {
          0 => TsTypeDef::keyword("never"),
          1 => non_null_union.remove(0),
          _ => TsTypeDef {
            union: Some(non_null_union),
            kind: Some(TsTypeDefKind::Union),
            ..Default::default()
          },
        })
      } else if with_null.keyword.is_some_and(|keyword| keyword == "null") {
        Some(TsTypeDef::keyword("never"))
      } else {
        None
      }
    }
    Expr::Bin(bin) => {
      // e.g.) export const foo = 1 == "bar";
      // e.g.) export const foo = 1 >> 1;
      match bin.op {
        BinaryOp::EqEq
        | BinaryOp::NotEq
        | BinaryOp::EqEqEq
        | BinaryOp::NotEqEq
        | BinaryOp::Lt
        | BinaryOp::LtEq
        | BinaryOp::Gt
        | BinaryOp::GtEq
        | BinaryOp::In
        | BinaryOp::InstanceOf => Some(TsTypeDef::bool_with_repr("boolean")),
        BinaryOp::LShift
        | BinaryOp::RShift
        | BinaryOp::ZeroFillRShift
        | BinaryOp::Sub
        | BinaryOp::Mul
        | BinaryOp::Div
        | BinaryOp::Mod
        | BinaryOp::BitOr
        | BinaryOp::BitXor
        | BinaryOp::BitAnd
        | BinaryOp::Exp => Some(TsTypeDef::number_with_repr("number")),
        BinaryOp::LogicalOr
        | BinaryOp::LogicalAnd
        | BinaryOp::NullishCoalescing
        | BinaryOp::Add => None,
      }
    }
    Expr::This(_)
    | Expr::Unary(_)
    | Expr::Assign(_)
    | Expr::Member(_)
    | Expr::SuperProp(_)
    | Expr::Seq(_)
    | Expr::Ident(_)
    | Expr::TaggedTpl(_)
    | Expr::Class(_)
    | Expr::Yield(_)
    | Expr::MetaProp(_)
    | Expr::JSXMember(_)
    | Expr::JSXNamespacedName(_)
    | Expr::JSXEmpty(_)
    | Expr::JSXElement(_)
    | Expr::JSXFragment(_)
    | Expr::TsInstantiation(_)
    | Expr::PrivateName(_)
    | Expr::OptChain(_)
    | Expr::Invalid(_) => None,
  }
}

pub fn infer_simple_ts_type_from_init(
  parsed_source: &ParsedSource,
  init: Option<&Expr>,
  is_const: bool,
) -> Option<TsTypeDef> {
  if let Some(init_expr) = init {
    infer_ts_type_from_expr(parsed_source, init_expr, is_const)
  } else {
    None
  }
}

fn infer_ts_type_from_arr_lit(
  parsed_source: &ParsedSource,
  arr_lit: &ArrayLit,
  is_const: bool,
) -> Option<TsTypeDef> {
  let mut defs = Vec::new();
  for expr in arr_lit.elems.iter().flatten() {
    if expr.spread.is_none() {
      if let Some(ts_type) =
        infer_ts_type_from_expr(parsed_source, &expr.expr, is_const)
      {
        if !defs.contains(&ts_type) {
          defs.push(ts_type);
        }
      } else {
        // it is not a trivial type that can be inferred an so will infer an
        // an any array.
        return Some(TsTypeDef {
          repr: "any[]".to_string(),
          kind: Some(TsTypeDefKind::Array),
          array: Some(Box::new(TsTypeDef::keyword("any"))),
          ..Default::default()
        });
      }
    } else {
      // TODO(@kitsonk) we should recursively unwrap the spread here
      return Some(TsTypeDef {
        repr: "any[]".to_string(),
        kind: Some(TsTypeDefKind::Array),
        array: Some(Box::new(TsTypeDef::keyword("any"))),
        ..Default::default()
      });
    }
  }
  match defs.len() {
    1 => Some(TsTypeDef {
      kind: Some(TsTypeDefKind::Array),
      array: Some(Box::new(defs[0].clone())),
      ..Default::default()
    }),
    2.. => {
      let union = TsTypeDef {
        kind: Some(TsTypeDefKind::Union),
        union: Some(defs),
        ..Default::default()
      };
      Some(TsTypeDef {
        kind: Some(TsTypeDefKind::Array),
        array: Some(Box::new(union)),
        ..Default::default()
      })
    }
    _ => None,
  }
}

fn infer_ts_type_from_arrow_expr(
  parsed_source: &ParsedSource,
  expr: &ArrowExpr,
) -> Option<TsTypeDef> {
  Some(TsTypeDef {
    kind: Some(TsTypeDefKind::FnOrConstructor),
    fn_or_constructor: Some(Box::new(TsFnOrConstructorDef::arrow_expr(
      parsed_source,
      expr,
    ))),
    ..Default::default()
  })
}

fn infer_ts_type_from_fn_expr(
  parsed_source: &ParsedSource,
  expr: &FnExpr,
) -> Option<TsTypeDef> {
  Some(TsTypeDef {
    kind: Some(TsTypeDefKind::FnOrConstructor),
    fn_or_constructor: Some(Box::new(TsFnOrConstructorDef::fn_expr(
      parsed_source,
      expr,
    ))),
    ..Default::default()
  })
}

fn infer_ts_type_from_const_assertion(
  parsed_source: &ParsedSource,
  assertion: &TsConstAssertion,
) -> Option<TsTypeDef> {
  match &*assertion.expr {
    Expr::Array(arr_lit) => {
      // e.g.) const n = ["a", 1] as const;
      infer_ts_type_from_arr_lit(parsed_source, arr_lit, true)
    }
    _ => infer_ts_type_from_expr(parsed_source, &assertion.expr, true),
  }
}

fn infer_ts_type_from_lit(lit: &Lit, is_const: bool) -> Option<TsTypeDef> {
  match lit {
    Lit::Num(num) => {
      if is_const {
        Some(TsTypeDef::number_literal(num))
      } else {
        Some(TsTypeDef::number_with_repr("number"))
      }
    }
    Lit::Str(str_) => {
      if is_const && str_.value.len() < 100 {
        Some(TsTypeDef::string_literal(str_))
      } else {
        Some(TsTypeDef::string_with_repr("string"))
      }
    }
    Lit::Bool(bool_) => {
      if is_const {
        Some(TsTypeDef::bool_literal(bool_))
      } else {
        Some(TsTypeDef::bool_with_repr("boolean"))
      }
    }
    Lit::BigInt(bigint_) => {
      if is_const {
        Some(TsTypeDef::bigint_literal(bigint_))
      } else {
        Some(TsTypeDef::bigint_with_repr("bigint"))
      }
    }
    Lit::Regex(regex) => Some(TsTypeDef::regexp(regex.exp.to_string())),
    Lit::Null(_null) => Some(TsTypeDef::keyword("null")),
    Lit::JSXText(_) => None,
  }
}

fn infer_ts_type_from_new_expr(
  parsed_source: &ParsedSource,
  new_expr: &NewExpr,
) -> Option<TsTypeDef> {
  match new_expr.callee.as_ref() {
    Expr::Ident(ident) => Some(TsTypeDef {
      repr: ident.sym.to_string(),
      kind: Some(TsTypeDefKind::TypeRef),
      type_ref: Some(TsTypeRefDef {
        type_params: new_expr.type_args.as_ref().map(|init| {
          maybe_type_param_instantiation_to_type_defs(parsed_source, Some(init))
        }),
        type_name: ident.sym.to_string(),
      }),
      ..Default::default()
    }),
    _ => None,
  }
}

fn infer_ts_type_from_call_expr(call_expr: &CallExpr) -> Option<TsTypeDef> {
  match &call_expr.callee {
    Callee::Expr(expr) => {
      if let Expr::Ident(ident) = expr.as_ref() {
        let sym = ident.sym.to_string();
        match sym.as_str() {
          "Symbol" | "Number" | "String" | "BigInt" => {
            Some(TsTypeDef::keyword_with_repr(
              &sym.to_ascii_lowercase(),
              &sym.clone(),
            ))
          }
          "Date" => Some(TsTypeDef::string_with_repr(&sym)),
          "RegExp" => Some(TsTypeDef::regexp(sym)),
          _ => None,
        }
      } else {
        None
      }
    }
    _ => None,
  }
}

fn infer_ts_type_from_obj(
  parsed_source: &ParsedSource,
  obj: &ObjectLit,
) -> Option<TsTypeDef> {
  let (methods, properties) = infer_ts_type_from_obj_inner(parsed_source, obj);
  if methods.is_empty() && properties.is_empty() {
    None
  } else {
    Some(TsTypeDef::object(methods, properties))
  }
}

fn infer_ts_type_from_obj_inner(
  parsed_source: &ParsedSource,
  obj: &ObjectLit,
) -> (Vec<MethodDef>, Vec<PropertyDef>) {
  let mut methods = Vec::<MethodDef>::new();
  let mut properties = Vec::<PropertyDef>::new();
  for obj_prop in &obj.props {
    match obj_prop {
      PropOrSpread::Prop(prop) => match &**prop {
        Prop::Shorthand(shorthand) => {
          // TODO(@crowlKats) we should pass previous nodes and take the type
          // from the previous symbol.
          properties.push(PropertyDef {
            name: shorthand.sym.to_string(),
            js_doc: Default::default(),
            location: get_location(parsed_source, shorthand.start()),
            params: vec![],
            readonly: false,
            computed: false,
            optional: false,
            ts_type: None,
            type_params: vec![],
          });
        }
        Prop::KeyValue(kv) => {
          properties.push(PropertyDef {
            name: prop_name_to_string(parsed_source, &kv.key),
            js_doc: Default::default(),
            location: get_location(parsed_source, kv.start()),
            params: vec![],
            readonly: false,
            computed: kv.key.is_computed(),
            optional: false,
            ts_type: infer_ts_type_from_expr(parsed_source, &kv.value, false),
            type_params: vec![],
          });
        }
        Prop::Assign(_) => unreachable!("This is invalid for object literal!"),
        Prop::Getter(getter) => {
          let name = prop_name_to_string(parsed_source, &getter.key);
          let computed = getter.key.is_computed();
          let return_type = getter
            .type_ann
            .as_ref()
            .map(|type_ann| TsTypeDef::new(parsed_source, &type_ann.type_ann));
          methods.push(MethodDef {
            name,
            js_doc: Default::default(),
            kind: MethodKind::Getter,
            location: get_location(parsed_source, getter.start()),
            params: vec![],
            computed,
            optional: false,
            return_type,
            type_params: vec![],
          });
        }
        Prop::Setter(setter) => {
          let name = prop_name_to_string(parsed_source, &setter.key);
          let computed = setter.key.is_computed();
          let param = pat_to_param_def(parsed_source, setter.param.as_ref());
          methods.push(MethodDef {
            name,
            js_doc: Default::default(),
            kind: MethodKind::Setter,
            location: get_location(parsed_source, setter.start()),
            params: vec![param],
            computed,
            optional: false,
            return_type: None,
            type_params: vec![],
          });
        }
        Prop::Method(method) => {
          let name = prop_name_to_string(parsed_source, &method.key);
          let computed = method.key.is_computed();
          let params = method
            .function
            .params
            .iter()
            .map(|param| param_to_param_def(parsed_source, param))
            .collect();
          let return_type =
            method.function.return_type.as_ref().map(|type_ann| {
              TsTypeDef::new(parsed_source, &type_ann.type_ann)
            });
          let type_params = maybe_type_param_decl_to_type_param_defs(
            parsed_source,
            method.function.type_params.as_deref(),
          );
          methods.push(MethodDef {
            name,
            js_doc: Default::default(),
            kind: MethodKind::Method,
            location: get_location(parsed_source, method.start()),
            params,
            computed,
            optional: false,
            return_type,
            type_params,
          });
        }
      },
      PropOrSpread::Spread(spread) => {
        if let Expr::Object(obj) = &*spread.expr {
          let (spread_methods, spread_properties) =
            infer_ts_type_from_obj_inner(parsed_source, obj);
          methods.extend(spread_methods);
          properties.extend(spread_properties);
        }
      }
    }
  }
  (methods, properties)
}

fn infer_ts_type_from_tpl(
  parsed_source: &ParsedSource,
  tpl: &Tpl,
  is_const: bool,
) -> TsTypeDef {
  let exprs = tpl
    .exprs
    .iter()
    .map(|expr| infer_ts_type_from_expr(parsed_source, expr, is_const))
    .collect::<Option<Vec<_>>>();

  if let Some(exprs) = exprs {
    TsTypeDef::tpl_literal(parsed_source, exprs, &tpl.quasis)
  } else {
    TsTypeDef::string_with_repr("string")
  }
}

impl Display for TsTypeDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    if self.kind.is_none() {
      return write!(f, "{}", colors::red("[UNSUPPORTED]"));
    }

    let kind = self.kind.as_ref().unwrap();
    match kind {
      TsTypeDefKind::Array => {
        let array = self.array.as_ref().unwrap();
        if matches!(
          array.kind,
          Some(TsTypeDefKind::Union) | Some(TsTypeDefKind::Intersection)
        ) {
          write!(f, "({})[]", self.array.as_ref().unwrap())
        } else {
          write!(f, "{}[]", self.array.as_ref().unwrap())
        }
      }
      TsTypeDefKind::Conditional => {
        let conditional = self.conditional_type.as_ref().unwrap();
        write!(
          f,
          "{} {} {} ? {} : {}",
          &*conditional.check_type,
          colors::magenta("extends"),
          &*conditional.extends_type,
          &*conditional.true_type,
          &*conditional.false_type
        )
      }
      TsTypeDefKind::Infer => {
        let infer = self.infer.as_ref().unwrap();
        write!(f, "{} {}", colors::magenta("infer"), infer.type_param)
      }
      TsTypeDefKind::ImportType => {
        let import_type = self.import_type.as_ref().unwrap();
        write!(f, "import(\"{}\")", import_type.specifier)?;
        if let Some(qualifier) = &import_type.qualifier {
          write!(f, ".{}", qualifier)?;
        }
        if let Some(type_params) = &import_type.type_params {
          write!(f, "<{}>", SliceDisplayer::new(type_params, ", ", false))?;
        }
        Ok(())
      }
      TsTypeDefKind::FnOrConstructor => {
        let fn_or_constructor = self.fn_or_constructor.as_ref().unwrap();
        write!(
          f,
          "{}({}) => {}",
          colors::magenta(if fn_or_constructor.constructor {
            "new "
          } else {
            ""
          }),
          SliceDisplayer::new(&fn_or_constructor.params, ", ", false),
          &fn_or_constructor.ts_type,
        )
      }
      TsTypeDefKind::IndexedAccess => {
        let indexed_access = self.indexed_access.as_ref().unwrap();
        write!(
          f,
          "{}[{}]",
          &*indexed_access.obj_type, &*indexed_access.index_type
        )
      }
      TsTypeDefKind::Intersection => {
        let intersection = self.intersection.as_ref().unwrap();
        write!(f, "{}", SliceDisplayer::new(intersection, " & ", false))
      }
      TsTypeDefKind::Mapped => {
        let mapped_type = self.mapped_type.as_ref().unwrap();
        let readonly = match mapped_type.readonly {
          Some(TruePlusMinus::True) => {
            format!("{} ", colors::magenta("readonly"))
          }
          Some(TruePlusMinus::Plus) => {
            format!("+{} ", colors::magenta("readonly"))
          }
          Some(TruePlusMinus::Minus) => {
            format!("-{} ", colors::magenta("readonly"))
          }
          _ => "".to_string(),
        };
        let optional = match mapped_type.optional {
          Some(TruePlusMinus::True) => "?",
          Some(TruePlusMinus::Plus) => "+?",
          Some(TruePlusMinus::Minus) => "-?",
          _ => "",
        };
        let type_param =
          if let Some(ts_type_def) = &mapped_type.type_param.constraint {
            format!("{} in {}", mapped_type.type_param.name, ts_type_def)
          } else {
            mapped_type.type_param.to_string()
          };
        let name_type = if let Some(name_type) = &mapped_type.name_type {
          format!(" {} {}", colors::magenta("as"), name_type)
        } else {
          "".to_string()
        };
        let ts_type = if let Some(ts_type) = &mapped_type.ts_type {
          format!(": {}", ts_type)
        } else {
          "".to_string()
        };
        write!(
          f,
          "{}[{}{}]{}{}",
          readonly, type_param, name_type, optional, ts_type
        )
      }
      TsTypeDefKind::Keyword => {
        write!(f, "{}", colors::cyan(self.keyword.as_ref().unwrap()))
      }
      TsTypeDefKind::Literal => {
        let literal = self.literal.as_ref().unwrap();
        match literal.kind {
          LiteralDefKind::Boolean => write!(
            f,
            "{}",
            colors::yellow(&literal.boolean.unwrap().to_string())
          ),
          LiteralDefKind::String => write!(
            f,
            "{}",
            colors::green(&format!("\"{}\"", literal.string.as_ref().unwrap()))
          ),
          LiteralDefKind::Template => {
            write!(f, "{}", colors::green("`"))?;
            for ts_type in literal.ts_types.as_ref().unwrap() {
              let kind = ts_type.kind.as_ref().unwrap();
              if *kind == TsTypeDefKind::Literal {
                let literal = ts_type.literal.as_ref().unwrap();
                if literal.kind == LiteralDefKind::String {
                  write!(
                    f,
                    "{}",
                    colors::green(literal.string.as_ref().unwrap())
                  )?;
                  continue;
                }
              }
              write!(
                f,
                "{}{}{}",
                colors::magenta("${"),
                ts_type,
                colors::magenta("}")
              )?;
            }
            write!(f, "{}", colors::green("`"))
          }
          LiteralDefKind::Number => write!(
            f,
            "{}",
            colors::yellow(&literal.number.unwrap().to_string())
          ),
          LiteralDefKind::BigInt => {
            write!(f, "{}", colors::yellow(&literal.string.as_ref().unwrap()))
          }
        }
      }
      TsTypeDefKind::Optional => {
        write!(f, "{}?", self.optional.as_ref().unwrap())
      }
      TsTypeDefKind::Parenthesized => {
        write!(f, "({})", self.parenthesized.as_ref().unwrap())
      }
      TsTypeDefKind::Rest => write!(f, "...{}", self.rest.as_ref().unwrap()),
      TsTypeDefKind::This => write!(f, "this"),
      TsTypeDefKind::Tuple => {
        let tuple = self.tuple.as_ref().unwrap();
        write!(f, "[{}]", SliceDisplayer::new(tuple, ", ", false))
      }
      TsTypeDefKind::TypeLiteral => {
        let type_literal = self.type_literal.as_ref().unwrap();
        write!(
          f,
          "{{ {}{}{}{}}}",
          SliceDisplayer::new(&type_literal.call_signatures, "; ", true),
          SliceDisplayer::new(&type_literal.methods, "; ", true),
          SliceDisplayer::new(&type_literal.properties, "; ", true),
          SliceDisplayer::new(&type_literal.index_signatures, "; ", true),
        )
      }
      TsTypeDefKind::TypeOperator => {
        let operator = self.type_operator.as_ref().unwrap();
        write!(f, "{} {}", operator.operator, &operator.ts_type)
      }
      TsTypeDefKind::TypeQuery => {
        write!(f, "typeof {}", self.type_query.as_ref().unwrap())
      }
      TsTypeDefKind::TypeRef => {
        let type_ref = self.type_ref.as_ref().unwrap();
        write!(f, "{}", colors::intense_blue(&type_ref.type_name))?;
        if let Some(type_params) = &type_ref.type_params {
          write!(f, "<{}>", SliceDisplayer::new(type_params, ", ", false))?;
        }
        Ok(())
      }
      TsTypeDefKind::Union => {
        let union = self.union.as_ref().unwrap();
        write!(f, "{}", SliceDisplayer::new(union, " | ", false))
      }
      TsTypeDefKind::TypePredicate => {
        let pred = self.type_predicate.as_ref().unwrap();
        write!(f, "{}", pred)
      }
    }
  }
}

pub fn maybe_type_param_instantiation_to_type_defs(
  parsed_source: &ParsedSource,
  maybe_type_param_instantiation: Option<&TsTypeParamInstantiation>,
) -> Vec<TsTypeDef> {
  if let Some(type_param_instantiation) = maybe_type_param_instantiation {
    type_param_instantiation
      .params
      .iter()
      .map(|type_param| TsTypeDef::new(parsed_source, type_param))
      .collect::<Vec<TsTypeDef>>()
  } else {
    vec![]
  }
}

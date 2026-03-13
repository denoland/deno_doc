// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::Location;
use crate::ParamDef;
use crate::display::SliceDisplayer;
use crate::display::display_computed;
use crate::display::display_optional;
use crate::display::display_readonly;
use crate::interface::expr_to_name;
use crate::node::Symbol;
use crate::params::ParamPatternDef;
use crate::params::formal_params_to_param_defs;
use crate::params::prop_name_to_string;
use crate::ts_type_param::TsTypeParamDef;
use crate::ts_type_param::maybe_type_param_decl_to_type_param_defs;
use crate::util::swc::get_location;
use crate::util::swc::is_false;
use crate::util::swc::js_doc_for_range;
use crate::util::types::MethodKind;
use crate::util::types::VarDeclKind;

use crate::function::FunctionDef;
use crate::js_doc::JsDoc;
use crate::node::DeclarationKind;
use crate::variable::VariableDef;
use deno_ast::oxc::ast::ast::*;
use deno_ast::oxc::span::GetSpan;
use deno_ast::oxc::syntax::operator::BinaryOperator;
use deno_graph::symbols::EsModuleInfo;
use deno_terminal::colors;
use serde::Deserialize;
use serde::Serialize;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;

/// Mirrors the old SWC `TruePlusMinus` enum with matching serde.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TruePlusMinus {
  True,
  Plus,
  Minus,
}

impl Serialize for TruePlusMinus {
  fn serialize<S: serde::Serializer>(
    &self,
    serializer: S,
  ) -> Result<S::Ok, S::Error> {
    match self {
      TruePlusMinus::True => serializer.serialize_bool(true),
      TruePlusMinus::Plus => serializer.serialize_str("plus"),
      TruePlusMinus::Minus => serializer.serialize_str("minus"),
    }
  }
}

impl<'de> Deserialize<'de> for TruePlusMinus {
  fn deserialize<D: serde::Deserializer<'de>>(
    deserializer: D,
  ) -> Result<Self, D::Error> {
    use serde::de;
    struct TruePlusMinusVisitor;
    impl<'de> de::Visitor<'de> for TruePlusMinusVisitor {
      type Value = TruePlusMinus;

      fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("true, \"plus\", or \"minus\"")
      }

      fn visit_bool<E: de::Error>(self, v: bool) -> Result<TruePlusMinus, E> {
        if v {
          Ok(TruePlusMinus::True)
        } else {
          Err(E::custom("expected true, got false"))
        }
      }

      fn visit_str<E: de::Error>(self, v: &str) -> Result<TruePlusMinus, E> {
        match v {
          "plus" => Ok(TruePlusMinus::Plus),
          "minus" => Ok(TruePlusMinus::Minus),
          _ => Err(E::custom(format!("unknown value: {}", v))),
        }
      }
    }
    deserializer.deserialize_any(TruePlusMinusVisitor)
  }
}

impl From<TSMappedTypeModifierOperator> for TruePlusMinus {
  fn from(op: TSMappedTypeModifierOperator) -> Self {
    match op {
      TSMappedTypeModifierOperator::True => TruePlusMinus::True,
      TSMappedTypeModifierOperator::Plus => TruePlusMinus::Plus,
      TSMappedTypeModifierOperator::Minus => TruePlusMinus::Minus,
    }
  }
}

impl TsTypeDef {
  fn ts_lit_type(module_info: &EsModuleInfo, other: &TSLiteralType) -> Self {
    match &other.literal {
      TSLiteral::NumericLiteral(num) => TsTypeDef::number_literal(num),
      TSLiteral::StringLiteral(str_) => {
        TsTypeDef::string_value(str_.value.to_string())
      }
      TSLiteral::TemplateLiteral(tpl) => TsTypeDef::tpl_literal(
        tpl
          .expressions
          .iter()
          .filter_map(|expr| infer_ts_type_from_expr(module_info, expr, true))
          .collect::<Vec<_>>(),
        &tpl.quasis,
      ),
      TSLiteral::BooleanLiteral(bool_) => TsTypeDef::bool_literal(bool_),
      TSLiteral::BigIntLiteral(bigint_) => TsTypeDef::bigint_literal(bigint_),
      TSLiteral::UnaryExpression(_) => TsTypeDef::keyword("number"),
    }
  }

  fn ts_template_literal_type(
    module_info: &EsModuleInfo,
    other: &TSTemplateLiteralType,
  ) -> Self {
    TsTypeDef::tpl_literal(
      other
        .types
        .iter()
        .map(|t| TsTypeDef::new(module_info, t))
        .collect::<Vec<_>>(),
      &other.quasis,
    )
  }

  fn ts_array_type(module_info: &EsModuleInfo, other: &TSArrayType) -> Self {
    let ts_type_def = TsTypeDef::new(module_info, &other.element_type);

    TsTypeDef {
      kind: TsTypeDefKind::Array(Box::new(ts_type_def)),
      repr: String::new(),
    }
  }

  fn ts_tuple_type(module_info: &EsModuleInfo, other: &TSTupleType) -> Self {
    let type_defs = other
      .element_types
      .iter()
      .map(|elem| ts_tuple_element_to_ts_type_def(module_info, elem))
      .collect::<Vec<_>>();

    TsTypeDef {
      kind: TsTypeDefKind::Tuple(type_defs),
      repr: String::new(),
    }
  }

  fn ts_union_type(module_info: &EsModuleInfo, other: &TSUnionType) -> Self {
    let types_union = other
      .types
      .iter()
      .map(|ts_type| TsTypeDef::new(module_info, ts_type))
      .collect::<Vec<_>>();

    TsTypeDef {
      kind: TsTypeDefKind::Union(types_union),
      repr: String::new(),
    }
  }

  fn ts_intersection_type(
    module_info: &EsModuleInfo,
    other: &TSIntersectionType,
  ) -> Self {
    let types_intersection = other
      .types
      .iter()
      .map(|ts_type| TsTypeDef::new(module_info, ts_type))
      .collect::<Vec<_>>();

    TsTypeDef {
      kind: TsTypeDefKind::Intersection(types_intersection),
      repr: String::new(),
    }
  }

  fn ts_type_operator(
    module_info: &EsModuleInfo,
    other: &TSTypeOperator,
  ) -> Self {
    let ts_type = TsTypeDef::new(module_info, &other.type_annotation);
    let operator = match other.operator {
      TSTypeOperatorOperator::Keyof => "keyof",
      TSTypeOperatorOperator::Unique => "unique",
      TSTypeOperatorOperator::Readonly => "readonly",
    };
    let type_operator_def = TsTypeOperatorDef {
      operator: operator.to_string(),
      ts_type,
    };

    TsTypeDef {
      kind: TsTypeDefKind::TypeOperator(Box::new(type_operator_def)),
      repr: String::new(),
    }
  }

  fn ts_parenthesized_type(
    module_info: &EsModuleInfo,
    other: &TSParenthesizedType,
  ) -> Self {
    let ts_type = TsTypeDef::new(module_info, &other.type_annotation);

    TsTypeDef {
      kind: TsTypeDefKind::Parenthesized(Box::new(ts_type)),
      repr: String::new(),
    }
  }

  fn ts_rest_type(module_info: &EsModuleInfo, other: &TSRestType) -> Self {
    let ts_type = TsTypeDef::new(module_info, &other.type_annotation);

    TsTypeDef {
      kind: TsTypeDefKind::Rest(Box::new(ts_type)),
      repr: String::new(),
    }
  }

  fn ts_optional_type(
    module_info: &EsModuleInfo,
    other: &TSOptionalType,
  ) -> Self {
    let ts_type = TsTypeDef::new(module_info, &other.type_annotation);

    TsTypeDef {
      kind: TsTypeDefKind::Optional(Box::new(ts_type)),
      repr: String::new(),
    }
  }

  fn ts_this_type(_other: &TSThisType) -> Self {
    TsTypeDef {
      repr: "this".to_string(),
      kind: TsTypeDefKind::This,
    }
  }

  fn ts_type_predicate(
    module_info: &EsModuleInfo,
    other: &TSTypePredicate,
  ) -> Self {
    let pred = TsTypePredicateDef {
      asserts: other.asserts,
      param: ts_type_predicate_name_to_this_or_ident(&other.parameter_name),
      r#type: other
        .type_annotation
        .as_ref()
        .map(|t| Box::new(TsTypeDef::new(module_info, &t.type_annotation))),
    };
    let mut repr_parts = Vec::new();
    if pred.asserts {
      repr_parts.push("asserts".to_string());
    }
    repr_parts.push(match &pred.param {
      ThisOrIdent::This => "this".to_string(),
      ThisOrIdent::Identifier { name } => name.clone(),
    });
    if let Some(ty) = &pred.r#type {
      repr_parts.push("is".to_string());
      repr_parts.push(ty.repr.clone());
    }
    TsTypeDef {
      repr: repr_parts.join(" "),
      kind: TsTypeDefKind::TypePredicate(pred),
    }
  }

  fn ts_type_query(other: &TSTypeQuery) -> Self {
    let type_name = ts_type_query_expr_name_to_name(&other.expr_name);

    TsTypeDef {
      repr: type_name.to_string(),
      kind: TsTypeDefKind::TypeQuery(type_name),
    }
  }

  fn ts_type_ref(module_info: &EsModuleInfo, other: &TSTypeReference) -> Self {
    let type_name = ts_type_name_to_name(&other.type_name);

    let type_params = if let Some(type_params_inst) = &other.type_arguments {
      let ts_type_defs = type_params_inst
        .params
        .iter()
        .map(|ts_type| TsTypeDef::new(module_info, ts_type))
        .collect::<Box<[_]>>();

      Some(ts_type_defs)
    } else {
      None
    };

    let resolution = ts_type_name_root_ident(&other.type_name)
      .and_then(|ident| resolve_type_ref(module_info, ident));

    TsTypeDef {
      repr: type_name.clone(),
      kind: TsTypeDefKind::TypeRef(TsTypeRefDef {
        type_params,
        type_name,
        resolution,
      }),
    }
  }

  pub fn ts_expr_with_type_args(
    module_info: &EsModuleInfo,
    other: &TSClassImplements,
  ) -> Self {
    let type_name = ts_type_name_to_name(&other.expression);

    let type_params = if let Some(type_params_inst) = &other.type_arguments {
      let ts_type_defs = type_params_inst
        .params
        .iter()
        .map(|ts_type| TsTypeDef::new(module_info, ts_type))
        .collect::<Box<[_]>>();

      Some(ts_type_defs)
    } else {
      None
    };

    let resolution = ts_type_name_root_ident(&other.expression)
      .and_then(|i| resolve_type_ref(module_info, i));

    TsTypeDef {
      repr: type_name.clone(),
      kind: TsTypeDefKind::TypeRef(TsTypeRefDef {
        type_params,
        type_name,
        resolution,
      }),
    }
  }

  pub fn ts_class_implements(
    module_info: &EsModuleInfo,
    other: &TSClassImplements,
  ) -> Self {
    Self::ts_expr_with_type_args(module_info, other)
  }

  pub fn ts_interface_heritage(
    module_info: &EsModuleInfo,
    other: &TSInterfaceHeritage,
  ) -> Self {
    let type_name = expr_to_name(&other.expression);

    let type_params = if let Some(type_params_inst) = &other.type_arguments {
      let ts_type_defs = type_params_inst
        .params
        .iter()
        .map(|ts_type| TsTypeDef::new(module_info, ts_type))
        .collect::<Box<[_]>>();

      Some(ts_type_defs)
    } else {
      None
    };

    let resolution = expr_root_ident(&other.expression)
      .and_then(|i| resolve_type_ref(module_info, i));

    TsTypeDef {
      repr: type_name.clone(),
      kind: TsTypeDefKind::TypeRef(TsTypeRefDef {
        type_params,
        type_name,
        resolution,
      }),
    }
  }

  fn ts_indexed_access_type(
    module_info: &EsModuleInfo,
    other: &TSIndexedAccessType,
  ) -> Self {
    let indexed_access_def = TsIndexedAccessDef {
      readonly: false,
      obj_type: Box::new(TsTypeDef::new(module_info, &other.object_type)),
      index_type: Box::new(TsTypeDef::new(module_info, &other.index_type)),
    };

    TsTypeDef {
      kind: TsTypeDefKind::IndexedAccess(indexed_access_def),
      repr: String::new(),
    }
  }

  fn ts_mapped_type(module_info: &EsModuleInfo, other: &TSMappedType) -> Self {
    let mapped_type_def = TsMappedTypeDef {
      readonly: other.readonly.map(TruePlusMinus::from),
      type_param: Box::new(TsTypeParamDef {
        name: other.key.name.to_string(),
        constraint: Some(TsTypeDef::new(module_info, &other.constraint)),
        default: None,
      }),
      name_type: other
        .name_type
        .as_ref()
        .map(|nt| Box::new(TsTypeDef::new(module_info, nt))),
      optional: other.optional.map(TruePlusMinus::from),
      ts_type: other
        .type_annotation
        .as_ref()
        .map(|a| Box::new(TsTypeDef::new(module_info, a))),
    };

    TsTypeDef {
      kind: TsTypeDefKind::Mapped(mapped_type_def),
      repr: String::new(),
    }
  }

  fn ts_type_lit(module_info: &EsModuleInfo, other: &TSTypeLiteral) -> Self {
    let mut constructors = vec![];
    let mut methods = vec![];
    let mut properties = vec![];
    let mut call_signatures = vec![];
    let mut index_signatures = vec![];

    for type_element in &other.members {
      match type_element {
        TSSignature::TSMethodSignature(ts_method_sig) => {
          let js_doc = js_doc_for_range(module_info, ts_method_sig.span)
            .unwrap_or_default();
          let params =
            formal_params_to_param_defs(module_info, &ts_method_sig.params);

          let maybe_return_type = ts_method_sig
            .return_type
            .as_ref()
            .map(|rt| TsTypeDef::new(module_info, &rt.type_annotation));

          let type_params = maybe_type_param_decl_to_type_param_defs(
            module_info,
            ts_method_sig.type_parameters.as_deref(),
          );
          let name = ts_method_sig
            .key
            .static_name()
            .map(|name| name.to_string())
            .unwrap_or_else(|| "[UNSUPPORTED]".to_string());
          let location = get_location(module_info, ts_method_sig.span.start);
          let method_def = MethodDef {
            name,
            js_doc,
            kind: MethodKind::from(ts_method_sig.kind),
            location,
            params,
            computed: ts_method_sig.computed,
            optional: ts_method_sig.optional,
            return_type: maybe_return_type,
            type_params,
          };
          methods.push(method_def);
        }
        TSSignature::TSPropertySignature(ts_prop_sig) => {
          let js_doc =
            js_doc_for_range(module_info, ts_prop_sig.span).unwrap_or_default();
          let name = ts_prop_sig
            .key
            .static_name()
            .map(|name| name.to_string())
            .unwrap_or_else(|| "[UNSUPPORTED]".to_string());

          let ts_type = ts_prop_sig
            .type_annotation
            .as_ref()
            .map(|rt| TsTypeDef::new(module_info, &rt.type_annotation));

          let type_params =
            maybe_type_param_decl_to_type_param_defs(module_info, None);
          let location = get_location(module_info, ts_prop_sig.span.start);
          let prop_def = PropertyDef {
            name,
            js_doc,
            location,
            params: vec![],
            ts_type,
            readonly: ts_prop_sig.readonly,
            computed: ts_prop_sig.computed,
            optional: ts_prop_sig.optional,
            type_params,
          };
          properties.push(prop_def);
        }
        TSSignature::TSCallSignatureDeclaration(ts_call_sig) => {
          let js_doc =
            js_doc_for_range(module_info, ts_call_sig.span).unwrap_or_default();
          let params =
            formal_params_to_param_defs(module_info, &ts_call_sig.params);

          let ts_type = ts_call_sig
            .return_type
            .as_ref()
            .map(|rt| TsTypeDef::new(module_info, &rt.type_annotation));

          let type_params = maybe_type_param_decl_to_type_param_defs(
            module_info,
            ts_call_sig.type_parameters.as_deref(),
          );

          let location = get_location(module_info, ts_call_sig.span.start);
          let call_sig_def = CallSignatureDef {
            js_doc,
            location,
            params,
            ts_type,
            type_params,
          };
          call_signatures.push(call_sig_def);
        }
        TSSignature::TSIndexSignature(ts_index_sig) => {
          let js_doc = js_doc_for_range(module_info, ts_index_sig.span)
            .unwrap_or_default();
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

          let location = get_location(module_info, ts_index_sig.span.start);
          let index_sig_def = IndexSignatureDef {
            js_doc,
            location,
            readonly: ts_index_sig.readonly,
            params,
            ts_type,
          };
          index_signatures.push(index_sig_def);
        }
        TSSignature::TSConstructSignatureDeclaration(ts_construct_sig) => {
          let js_doc = js_doc_for_range(module_info, ts_construct_sig.span)
            .unwrap_or_default();
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

          let location = get_location(module_info, ts_construct_sig.span.start);
          let construct_sig_def = ConstructorDef {
            js_doc,
            location,
            params,
            return_type: maybe_return_type,
            type_params,
          };

          constructors.push(construct_sig_def);
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
      kind: TsTypeDefKind::TypeLiteral(type_literal),
      repr: String::new(),
    }
  }

  fn ts_conditional_type(
    module_info: &EsModuleInfo,
    other: &TSConditionalType,
  ) -> Self {
    let conditional_type_def = TsConditionalDef {
      check_type: Box::new(TsTypeDef::new(module_info, &other.check_type)),
      extends_type: Box::new(TsTypeDef::new(module_info, &other.extends_type)),
      true_type: Box::new(TsTypeDef::new(module_info, &other.true_type)),
      false_type: Box::new(TsTypeDef::new(module_info, &other.false_type)),
    };

    TsTypeDef {
      kind: TsTypeDefKind::Conditional(conditional_type_def),
      repr: String::new(),
    }
  }

  fn ts_infer_type(module_info: &EsModuleInfo, other: &TSInferType) -> Self {
    let infer = TsInferDef {
      type_param: Box::new(TsTypeParamDef::new(
        module_info,
        &other.type_parameter,
      )),
    };

    TsTypeDef {
      kind: TsTypeDefKind::Infer(infer),
      repr: String::new(),
    }
  }

  fn ts_import_type(module_info: &EsModuleInfo, other: &TSImportType) -> Self {
    let type_params = if let Some(type_params_inst) = &other.type_arguments {
      let ts_type_defs = type_params_inst
        .params
        .iter()
        .map(|param| TsTypeDef::new(module_info, param))
        .collect::<Vec<_>>();

      Some(ts_type_defs)
    } else {
      None
    };

    let import_type_def = TsImportTypeDef {
      specifier: other.source.value.to_string(),
      qualifier: other
        .qualifier
        .as_ref()
        .map(ts_import_type_qualifier_to_name),
      type_params,
    };

    TsTypeDef {
      kind: TsTypeDefKind::ImportType(import_type_def),
      repr: String::new(),
    }
  }

  fn ts_fn_type(
    module_info: &EsModuleInfo,
    ts_fn_type: &TSFunctionType,
  ) -> Self {
    let params = formal_params_to_param_defs(module_info, &ts_fn_type.params);

    let type_params = maybe_type_param_decl_to_type_param_defs(
      module_info,
      ts_fn_type.type_parameters.as_deref(),
    );

    let fn_def = TsFnOrConstructorDef {
      constructor: false,
      ts_type: TsTypeDef::new(
        module_info,
        &ts_fn_type.return_type.type_annotation,
      ),
      params,
      type_params,
    };

    TsTypeDef {
      kind: TsTypeDefKind::FnOrConstructor(Box::new(fn_def)),
      repr: String::new(),
    }
  }

  fn ts_constructor_type(
    module_info: &EsModuleInfo,
    ctor_type: &TSConstructorType,
  ) -> Self {
    let params = formal_params_to_param_defs(module_info, &ctor_type.params);

    let type_params = maybe_type_param_decl_to_type_param_defs(
      module_info,
      ctor_type.type_parameters.as_deref(),
    );

    let fn_def = TsFnOrConstructorDef {
      constructor: true,
      ts_type: TsTypeDef::new(
        module_info,
        &ctor_type.return_type.type_annotation,
      ),
      params,
      type_params,
    };

    TsTypeDef {
      kind: TsTypeDefKind::FnOrConstructor(Box::new(fn_def)),
      repr: String::new(),
    }
  }

  fn ts_named_tuple_member(
    module_info: &EsModuleInfo,
    other: &TSNamedTupleMember,
  ) -> Self {
    ts_tuple_element_to_ts_type_def(module_info, &other.element_type)
  }

  pub fn new(module_info: &EsModuleInfo, other: &TSType) -> Self {
    match other {
      TSType::TSAnyKeyword(_) => TsTypeDef::keyword("any"),
      TSType::TSUnknownKeyword(_) => TsTypeDef::keyword("unknown"),
      TSType::TSNumberKeyword(_) => TsTypeDef::keyword("number"),
      TSType::TSObjectKeyword(_) => TsTypeDef::keyword("object"),
      TSType::TSBooleanKeyword(_) => TsTypeDef::keyword("boolean"),
      TSType::TSBigIntKeyword(_) => TsTypeDef::keyword("bigint"),
      TSType::TSStringKeyword(_) => TsTypeDef::keyword("string"),
      TSType::TSSymbolKeyword(_) => TsTypeDef::keyword("symbol"),
      TSType::TSVoidKeyword(_) => TsTypeDef::keyword("void"),
      TSType::TSUndefinedKeyword(_) => TsTypeDef::keyword("undefined"),
      TSType::TSNullKeyword(_) => TsTypeDef::keyword("null"),
      TSType::TSNeverKeyword(_) => TsTypeDef::keyword("never"),
      TSType::TSIntrinsicKeyword(_) => TsTypeDef::keyword("intrinsic"),
      TSType::TSThisType(this_type) => TsTypeDef::ts_this_type(this_type),
      TSType::TSFunctionType(fn_type) => {
        TsTypeDef::ts_fn_type(module_info, fn_type)
      }
      TSType::TSConstructorType(ctor_type) => {
        TsTypeDef::ts_constructor_type(module_info, ctor_type)
      }
      TSType::TSTypeReference(type_ref) => {
        TsTypeDef::ts_type_ref(module_info, type_ref)
      }
      TSType::TSTypeQuery(type_query) => TsTypeDef::ts_type_query(type_query),
      TSType::TSTypeLiteral(type_literal) => {
        TsTypeDef::ts_type_lit(module_info, type_literal)
      }
      TSType::TSArrayType(array_type) => {
        TsTypeDef::ts_array_type(module_info, array_type)
      }
      TSType::TSTupleType(tuple_type) => {
        TsTypeDef::ts_tuple_type(module_info, tuple_type)
      }
      TSType::TSUnionType(union_type) => {
        TsTypeDef::ts_union_type(module_info, union_type)
      }
      TSType::TSIntersectionType(intersection_type) => {
        TsTypeDef::ts_intersection_type(module_info, intersection_type)
      }
      TSType::TSConditionalType(conditional_type) => {
        TsTypeDef::ts_conditional_type(module_info, conditional_type)
      }
      TSType::TSInferType(infer_type) => {
        TsTypeDef::ts_infer_type(module_info, infer_type)
      }
      TSType::TSParenthesizedType(paren_type) => {
        TsTypeDef::ts_parenthesized_type(module_info, paren_type)
      }
      TSType::TSTypeOperatorType(type_op_type) => {
        TsTypeDef::ts_type_operator(module_info, type_op_type)
      }
      TSType::TSIndexedAccessType(indexed_access_type) => {
        TsTypeDef::ts_indexed_access_type(module_info, indexed_access_type)
      }
      TSType::TSMappedType(mapped_type) => {
        TsTypeDef::ts_mapped_type(module_info, mapped_type)
      }
      TSType::TSLiteralType(lit_type) => {
        TsTypeDef::ts_lit_type(module_info, lit_type)
      }
      TSType::TSTypePredicate(type_predicate_type) => {
        TsTypeDef::ts_type_predicate(module_info, type_predicate_type)
      }
      TSType::TSImportType(import_type) => {
        TsTypeDef::ts_import_type(module_info, import_type)
      }
      TSType::TSNamedTupleMember(named_tuple_member) => {
        TsTypeDef::ts_named_tuple_member(module_info, named_tuple_member)
      }
      TSType::TSTemplateLiteralType(tpl_type) => {
        TsTypeDef::ts_template_literal_type(module_info, tpl_type)
      }
      TSType::JSDocNullableType(_)
      | TSType::JSDocNonNullableType(_)
      | TSType::JSDocUnknownType(_) => TsTypeDef::keyword("unknown"),
    }
  }
}

type Id = (String, usize);

fn identifier_reference_to_id(
  module_info: &EsModuleInfo,
  ident: &IdentifierReference,
) -> Id {
  let ctxt = ident
    .reference_id
    .get()
    .zip(module_info.scoping())
    .and_then(|(reference_id, scoping)| {
      scoping.get_reference(reference_id).symbol_id()
    })
    .map(|symbol_id| symbol_id.index() + 1)
    .unwrap_or(0);
  (ident.name.to_string(), ctxt)
}

fn ts_type_name_to_name(type_name: &TSTypeName) -> String {
  match type_name {
    TSTypeName::IdentifierReference(ident) => ident.name.to_string(),
    TSTypeName::QualifiedName(ts_qualified_name) => {
      let left = ts_type_name_to_name(&ts_qualified_name.left);
      let right = ts_qualified_name.right.name.to_string();
      format!("{}.{}", left, right)
    }
    TSTypeName::ThisExpression(_) => "this".to_string(),
  }
}

fn ts_type_name_root_ident<'a>(
  type_name: &'a TSTypeName<'a>,
) -> Option<&'a IdentifierReference<'a>> {
  match type_name {
    TSTypeName::IdentifierReference(ident) => Some(ident),
    TSTypeName::QualifiedName(ts_qualified_name) => {
      ts_type_name_root_ident(&ts_qualified_name.left)
    }
    TSTypeName::ThisExpression(_) => None,
  }
}

fn ts_type_query_expr_name_to_name(expr_name: &TSTypeQueryExprName) -> String {
  match expr_name {
    TSTypeQueryExprName::TSImportType(import_type) => {
      import_type.source.value.to_string()
    }
    TSTypeQueryExprName::IdentifierReference(ident) => ident.name.to_string(),
    TSTypeQueryExprName::QualifiedName(qualified) => {
      let left = ts_type_name_to_name(&qualified.left);
      let right = qualified.right.name.to_string();
      format!("{}.{}", left, right)
    }
    TSTypeQueryExprName::ThisExpression(_) => "this".to_string(),
  }
}

fn ts_import_type_qualifier_to_name(
  qualifier: &TSImportTypeQualifier,
) -> String {
  match qualifier {
    TSImportTypeQualifier::Identifier(ident) => ident.name.to_string(),
    TSImportTypeQualifier::QualifiedName(qualified) => {
      let left = ts_import_type_qualifier_to_name(&qualified.left);
      let right = qualified.right.name.to_string();
      format!("{}.{}", left, right)
    }
  }
}

fn ts_type_predicate_name_to_this_or_ident(
  name: &TSTypePredicateName,
) -> ThisOrIdent {
  match name {
    TSTypePredicateName::This(_) => ThisOrIdent::This,
    TSTypePredicateName::Identifier(ident) => ThisOrIdent::Identifier {
      name: ident.name.to_string(),
    },
  }
}

fn expr_root_ident<'a>(
  expr: &'a Expression<'a>,
) -> Option<&'a IdentifierReference<'a>> {
  match expr {
    Expression::Identifier(ident) => Some(ident),
    Expression::StaticMemberExpression(member_expr) => {
      expr_root_ident(&member_expr.object)
    }
    Expression::ComputedMemberExpression(member_expr) => {
      expr_root_ident(&member_expr.object)
    }
    _ => None,
  }
}

fn resolve_type_ref(
  module_info: &EsModuleInfo,
  ident: &IdentifierReference,
) -> Option<TypeRefResolution> {
  let id = identifier_reference_to_id(module_info, ident);
  if let Some(symbol) = module_info.symbol_from_swc(&id) {
    if let Some(file_dep) = symbol.file_dep() {
      Some(TypeRefResolution::Import {
        specifier: file_dep.specifier.clone(),
        name: file_dep.name.maybe_name().map(|s| s.to_string()),
      })
    } else {
      Some(TypeRefResolution::Local)
    }
  } else if id.1 != 0 {
    Some(TypeRefResolution::TypeParam {
      declaring_name: None,
      declaring_kind: None,
    })
  } else {
    None
  }
}

fn ts_tuple_element_to_ts_type_def(
  module_info: &EsModuleInfo,
  elem: &TSTupleElement,
) -> TsTypeDef {
  match elem {
    TSTupleElement::TSOptionalType(opt) => {
      TsTypeDef::ts_optional_type(module_info, opt)
    }
    TSTupleElement::TSRestType(rest) => {
      TsTypeDef::ts_rest_type(module_info, rest)
    }
    TSTupleElement::TSAnyKeyword(_) => TsTypeDef::keyword("any"),
    TSTupleElement::TSUnknownKeyword(_) => TsTypeDef::keyword("unknown"),
    TSTupleElement::TSNumberKeyword(_) => TsTypeDef::keyword("number"),
    TSTupleElement::TSObjectKeyword(_) => TsTypeDef::keyword("object"),
    TSTupleElement::TSBooleanKeyword(_) => TsTypeDef::keyword("boolean"),
    TSTupleElement::TSBigIntKeyword(_) => TsTypeDef::keyword("bigint"),
    TSTupleElement::TSStringKeyword(_) => TsTypeDef::keyword("string"),
    TSTupleElement::TSSymbolKeyword(_) => TsTypeDef::keyword("symbol"),
    TSTupleElement::TSVoidKeyword(_) => TsTypeDef::keyword("void"),
    TSTupleElement::TSUndefinedKeyword(_) => TsTypeDef::keyword("undefined"),
    TSTupleElement::TSNullKeyword(_) => TsTypeDef::keyword("null"),
    TSTupleElement::TSNeverKeyword(_) => TsTypeDef::keyword("never"),
    TSTupleElement::TSIntrinsicKeyword(_) => TsTypeDef::keyword("intrinsic"),
    TSTupleElement::TSThisType(t) => TsTypeDef::ts_this_type(t),
    TSTupleElement::TSFunctionType(t) => TsTypeDef::ts_fn_type(module_info, t),
    TSTupleElement::TSConstructorType(t) => {
      TsTypeDef::ts_constructor_type(module_info, t)
    }
    TSTupleElement::TSTypeReference(t) => {
      TsTypeDef::ts_type_ref(module_info, t)
    }
    TSTupleElement::TSTypeQuery(t) => TsTypeDef::ts_type_query(t),
    TSTupleElement::TSTypeLiteral(t) => TsTypeDef::ts_type_lit(module_info, t),
    TSTupleElement::TSArrayType(t) => TsTypeDef::ts_array_type(module_info, t),
    TSTupleElement::TSTupleType(t) => TsTypeDef::ts_tuple_type(module_info, t),
    TSTupleElement::TSUnionType(t) => TsTypeDef::ts_union_type(module_info, t),
    TSTupleElement::TSIntersectionType(t) => {
      TsTypeDef::ts_intersection_type(module_info, t)
    }
    TSTupleElement::TSConditionalType(t) => {
      TsTypeDef::ts_conditional_type(module_info, t)
    }
    TSTupleElement::TSInferType(t) => TsTypeDef::ts_infer_type(module_info, t),
    TSTupleElement::TSParenthesizedType(t) => {
      TsTypeDef::ts_parenthesized_type(module_info, t)
    }
    TSTupleElement::TSTypeOperatorType(t) => {
      TsTypeDef::ts_type_operator(module_info, t)
    }
    TSTupleElement::TSIndexedAccessType(t) => {
      TsTypeDef::ts_indexed_access_type(module_info, t)
    }
    TSTupleElement::TSMappedType(t) => {
      TsTypeDef::ts_mapped_type(module_info, t)
    }
    TSTupleElement::TSLiteralType(t) => TsTypeDef::ts_lit_type(module_info, t),
    TSTupleElement::TSTypePredicate(t) => {
      TsTypeDef::ts_type_predicate(module_info, t)
    }
    TSTupleElement::TSImportType(t) => {
      TsTypeDef::ts_import_type(module_info, t)
    }
    TSTupleElement::TSNamedTupleMember(t) => {
      TsTypeDef::ts_named_tuple_member(module_info, t)
    }
    TSTupleElement::TSTemplateLiteralType(t) => {
      TsTypeDef::ts_template_literal_type(module_info, t)
    }
    TSTupleElement::JSDocNullableType(_)
    | TSTupleElement::JSDocNonNullableType(_)
    | TSTupleElement::JSDocUnknownType(_) => TsTypeDef::keyword("unknown"),
  }
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum TypeParamDeclaringKind {
  Class,
  Interface,
  Function,
  TypeAlias,
  Method,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum TypeRefResolution {
  /// Resolves to a symbol defined locally in this module.
  Local,
  /// Resolves to a type parameter in the enclosing declaration.
  #[serde(rename_all = "camelCase")]
  TypeParam {
    /// Name of the declaration that declares this type parameter.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    declaring_name: Option<String>,
    /// Kind of the declaration that declares this type parameter.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    declaring_kind: Option<TypeParamDeclaringKind>,
  },
  /// Resolves to a symbol imported from another module.
  #[serde(rename_all = "camelCase")]
  Import {
    /// The raw import specifier (e.g. `"./bar.ts"` or `"https://..."`).
    specifier: String,
    /// The name of the symbol in the source module
    /// (e.g. `"Foo"` for `import { Foo }`, `"default"` for default imports,
    /// or `None` for namespace imports like `import * as ns`).
    #[serde(skip_serializing_if = "Option::is_none", default)]
    name: Option<String>,
  },
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct TsTypeRefDef {
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub type_params: Option<Box<[TsTypeDef]>>,
  pub type_name: String,
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub resolution: Option<TypeRefResolution>,
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
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub type_params: Box<[TsTypeParamDef]>,
}

impl TsFnOrConstructorDef {
  fn arrow_expr(
    module_info: &EsModuleInfo,
    expr: &ArrowFunctionExpression,
  ) -> Self {
    let params = formal_params_to_param_defs(module_info, &expr.params);
    let ts_type = expr
      .return_type
      .as_deref()
      .map(|return_type| {
        TsTypeDef::new(module_info, &return_type.type_annotation)
      })
      .unwrap_or_else(|| TsTypeDef::keyword("unknown"));
    let type_params = maybe_type_param_decl_to_type_param_defs(
      module_info,
      expr.type_parameters.as_deref(),
    );

    Self {
      constructor: false,
      ts_type,
      params,
      type_params,
    }
  }

  fn fn_expr(module_info: &EsModuleInfo, expr: &Function) -> Self {
    let params = formal_params_to_param_defs(module_info, &expr.params);
    let ts_type = expr
      .return_type
      .as_deref()
      .map(|return_type| {
        TsTypeDef::new(module_info, &return_type.type_annotation)
      })
      .unwrap_or_else(|| TsTypeDef::keyword("unknown"));
    let type_params = maybe_type_param_decl_to_type_param_defs(
      module_info,
      expr.type_parameters.as_deref(),
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
  #[serde(skip_serializing_if = "is_false", default)]
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

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ConstructorDef {
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,
  pub params: Vec<ParamDef>,
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub return_type: Option<TsTypeDef>,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub type_params: Box<[TsTypeParamDef]>,
  pub location: Location,
}

impl PartialEq for ConstructorDef {
  fn eq(&self, other: &Self) -> bool {
    self.params == other.params
      && self.return_type == other.return_type
      && self.type_params == other.type_params
  }
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

#[derive(Debug, Serialize, Deserialize, Clone)]
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
  #[serde(skip_serializing_if = "is_false", default)]
  pub optional: bool,
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub return_type: Option<TsTypeDef>,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub type_params: Box<[TsTypeParamDef]>,
}

impl PartialEq for MethodDef {
  fn eq(&self, other: &Self) -> bool {
    self.name == other.name
      && self.kind == other.kind
      && self.params == other.params
      && self.computed == other.computed
      && self.optional == other.optional
      && self.return_type == other.return_type
      && self.type_params == other.type_params
  }
}

impl From<MethodDef> for Symbol {
  fn from(def: MethodDef) -> Symbol {
    Symbol::function(
      def.name.into_boxed_str(),
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
        decorators: Box::new([]),
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

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct PropertyDef {
  pub name: String,
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,
  #[serde(default)]
  pub location: Location,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub params: Vec<ParamDef>,
  #[serde(skip_serializing_if = "is_false", default)]
  pub readonly: bool,
  #[serde(skip_serializing_if = "is_false", default)]
  pub computed: bool,
  #[serde(skip_serializing_if = "is_false", default)]
  pub optional: bool,
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub ts_type: Option<TsTypeDef>,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub type_params: Box<[TsTypeParamDef]>,
}

impl PartialEq for PropertyDef {
  fn eq(&self, other: &Self) -> bool {
    self.name == other.name
      && self.params == other.params
      && self.readonly == other.readonly
      && self.computed == other.computed
      && self.optional == other.optional
      && self.ts_type == other.ts_type
      && self.type_params == other.type_params
  }
}

impl From<PropertyDef> for Symbol {
  fn from(def: PropertyDef) -> Symbol {
    Symbol::variable(
      def.name.into_boxed_str(),
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

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct CallSignatureDef {
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,
  #[serde(default)]
  pub location: Location,
  pub params: Vec<ParamDef>,
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub ts_type: Option<TsTypeDef>,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub type_params: Box<[TsTypeParamDef]>,
}

impl PartialEq for CallSignatureDef {
  fn eq(&self, other: &Self) -> bool {
    self.params == other.params
      && self.ts_type == other.ts_type
      && self.type_params == other.type_params
  }
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

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct IndexSignatureDef {
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,
  #[serde(skip_serializing_if = "is_false", default)]
  pub readonly: bool,
  pub params: Vec<ParamDef>,
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub ts_type: Option<TsTypeDef>,
  #[serde(default)]
  pub location: Location,
}

impl PartialEq for IndexSignatureDef {
  fn eq(&self, other: &Self) -> bool {
    self.readonly == other.readonly
      && self.params == other.params
      && self.ts_type == other.ts_type
  }
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
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
#[serde(tag = "kind", content = "value", rename_all = "camelCase")]
pub enum TsTypeDefKind {
  Keyword(String),
  Literal(LiteralDef),
  TypeRef(TsTypeRefDef),
  Union(Vec<TsTypeDef>),
  Intersection(Vec<TsTypeDef>),
  Array(Box<TsTypeDef>),
  Tuple(Vec<TsTypeDef>),
  TypeOperator(Box<TsTypeOperatorDef>),
  Parenthesized(Box<TsTypeDef>),
  Rest(Box<TsTypeDef>),
  Optional(Box<TsTypeDef>),
  TypeQuery(String),
  This,
  FnOrConstructor(Box<TsFnOrConstructorDef>),
  Conditional(TsConditionalDef),
  Infer(TsInferDef),
  IndexedAccess(TsIndexedAccessDef),
  Mapped(TsMappedTypeDef),
  TypeLiteral(TsTypeLiteralDef),
  TypePredicate(TsTypePredicateDef),
  ImportType(TsImportTypeDef),
  Unsupported,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct TsTypeDef {
  #[serde(skip_serializing_if = "String::is_empty", default)]
  pub repr: String,
  #[serde(flatten)]
  pub kind: TsTypeDefKind,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum ThisOrIdent {
  This,
  Identifier { name: String },
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
  #[serde(skip_serializing_if = "is_false", default)]
  pub asserts: bool,

  /// (2) The term of predicate
  pub param: ThisOrIdent,

  /// (3) The type against which the parameter is checked
  #[serde(skip_serializing_if = "Option::is_none", default)]
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
  pub fn number_literal(num: &NumericLiteral) -> Self {
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

  pub fn string_literal(str_node: &StringLiteral) -> Self {
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
    types: Vec<TsTypeDef>,
    quasis: &[TemplateElement],
  ) -> Self {
    let mut types_out: Vec<(Self, String)> = Vec::new();
    for ts_type in types {
      let repr = format!("${{{}}}", ts_type);
      types_out.push((ts_type, repr))
    }
    let mut qasis_out: Vec<(Self, String)> = Vec::new();
    for quasi in quasis {
      let repr = quasi.value.raw.to_string();
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

  pub fn bool_literal(bool_node: &BooleanLiteral) -> Self {
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

  pub fn bigint_literal(bigint_node: &BigIntLiteral) -> Self {
    let repr = bigint_node
      .raw
      .as_ref()
      .map(|r| {
        let s = r.as_str();
        s.strip_suffix('n').unwrap_or(s).to_string()
      })
      .unwrap_or_default();
    let lit = LiteralDef {
      kind: LiteralDefKind::BigInt,
      number: None,
      string: Some(repr.clone()),
      ts_types: None,
      boolean: None,
    };
    Self::literal(repr, lit)
  }

  pub fn regexp(repr: String) -> Self {
    Self {
      repr,
      kind: TsTypeDefKind::TypeRef(TsTypeRefDef {
        type_params: None,
        type_name: "RegExp".to_string(),
        resolution: None,
      }),
    }
  }

  pub fn object(methods: Vec<MethodDef>, properties: Vec<PropertyDef>) -> Self {
    Self {
      repr: "".to_string(),
      kind: TsTypeDefKind::TypeLiteral(TsTypeLiteralDef {
        methods,
        properties,
        ..Default::default()
      }),
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
      kind: TsTypeDefKind::Keyword(keyword_str.to_string()),
    }
  }

  fn literal(repr: String, lit: LiteralDef) -> Self {
    Self {
      repr,
      kind: TsTypeDefKind::Literal(lit),
    }
  }
}

pub(crate) fn infer_ts_type_from_expr(
  module_info: &EsModuleInfo,
  expr: &Expression,
  is_const: bool,
) -> Option<TsTypeDef> {
  match expr {
    Expression::ArrayExpression(arr_lit) => {
      // e.g.) const n = ["a", 1];
      infer_ts_type_from_arr_lit(module_info, arr_lit, false)
    }
    Expression::ArrowFunctionExpression(expr) => {
      // e.g.) const f = (a: string): void => {};
      infer_ts_type_from_arrow_expr(module_info, expr)
    }
    Expression::FunctionExpression(expr) => {
      // e.g.) const f = function a(a:string): void {};
      infer_ts_type_from_fn_expr(module_info, expr)
    }
    Expression::NumericLiteral(num) => {
      if is_const {
        Some(TsTypeDef::number_literal(num))
      } else {
        Some(TsTypeDef::number_with_repr("number"))
      }
    }
    Expression::StringLiteral(str_) => {
      if is_const && str_.value.len() < 100 {
        Some(TsTypeDef::string_literal(str_))
      } else {
        Some(TsTypeDef::string_with_repr("string"))
      }
    }
    Expression::BooleanLiteral(bool_) => {
      if is_const {
        Some(TsTypeDef::bool_literal(bool_))
      } else {
        Some(TsTypeDef::bool_with_repr("boolean"))
      }
    }
    Expression::BigIntLiteral(bigint_) => {
      if is_const {
        Some(TsTypeDef::bigint_literal(bigint_))
      } else {
        Some(TsTypeDef::bigint_with_repr("bigint"))
      }
    }
    Expression::RegExpLiteral(regex) => {
      Some(TsTypeDef::regexp(regex.regex.pattern.text.to_string()))
    }
    Expression::NullLiteral(_) => Some(TsTypeDef::keyword("null")),
    Expression::NewExpression(expr) => {
      // e.g.) const d = new Date()
      infer_ts_type_from_new_expr(module_info, expr)
    }
    Expression::TemplateLiteral(tpl) => {
      // e.g.) const s = `hello`;
      Some(infer_ts_type_from_tpl(module_info, tpl, is_const))
    }
    Expression::TSAsExpression(as_expr) => {
      if is_const_assertion(&as_expr.type_annotation) {
        match &as_expr.expression {
          Expression::ArrayExpression(arr) => {
            infer_ts_type_from_arr_lit(module_info, arr, true)
          }
          _ => infer_ts_type_from_expr(module_info, &as_expr.expression, true),
        }
      } else {
        Some(TsTypeDef::new(module_info, &as_expr.type_annotation))
      }
    }
    Expression::CallExpression(expr) => {
      // e.g.) const value = Number(123);
      infer_ts_type_from_call_expr(expr)
    }
    Expression::ObjectExpression(obj) => {
      // e.g.) const value = {foo: "bar"};
      infer_ts_type_from_obj(module_info, obj)
    }
    Expression::TSSatisfiesExpression(satisfies) => {
      // e.g.) const value = {foo: "bar"} satifies Record<string, string>;
      infer_ts_type_from_expr(module_info, &satisfies.expression, is_const)
    }
    Expression::UpdateExpression(_) => {
      // e.g.) let foo = 0;
      //       const bar = foo++;
      Some(TsTypeDef::number_with_repr("number"))
    }
    Expression::TSTypeAssertion(assertion) => {
      // e.g.) export const foo = <string> 1;
      Some(TsTypeDef::new(module_info, &assertion.type_annotation))
    }
    Expression::ParenthesizedExpression(paren) => {
      // e.g.) export const foo = (1);
      infer_ts_type_from_expr(module_info, &paren.expression, is_const)
    }
    Expression::AwaitExpression(await_expr) => {
      // e.g.) export const foo = await 1;
      infer_ts_type_from_expr(module_info, &await_expr.argument, is_const)
    }
    Expression::ConditionalExpression(cond) => {
      // e.g.) export const foo = true ? "a" : 1;
      let left =
        infer_ts_type_from_expr(module_info, &cond.consequent, is_const)?;
      let right =
        infer_ts_type_from_expr(module_info, &cond.alternate, is_const)?;

      Some(TsTypeDef {
        kind: TsTypeDefKind::Union(vec![left, right]),
        repr: String::new(),
      })
    }
    Expression::TSNonNullExpression(non_null) => {
      // e.g.) export const foo = (true ? "a" : null)!;
      // e.g.) export const foo = null!;
      let with_null =
        infer_ts_type_from_expr(module_info, &non_null.expression, is_const)?;

      if let TsTypeDefKind::Union(union) = with_null.kind {
        let mut non_null_union = union
          .into_iter()
          .filter(|item| {
            if let TsTypeDefKind::Keyword(keyword) = &item.kind {
              return keyword != "null";
            }

            true
          })
          .collect::<Vec<_>>();

        Some(match non_null_union.len() {
          0 => TsTypeDef::keyword("never"),
          1 => non_null_union.remove(0),
          _ => TsTypeDef {
            kind: TsTypeDefKind::Union(non_null_union),
            repr: String::new(),
          },
        })
      } else if let TsTypeDefKind::Keyword(keyword) = with_null.kind
        && keyword == "null"
      {
        Some(TsTypeDef::keyword("never"))
      } else {
        None
      }
    }
    Expression::BinaryExpression(bin) => {
      // e.g.) export const foo = 1 == "bar";
      // e.g.) export const foo = 1 >> 1;
      match bin.operator {
        BinaryOperator::Equality
        | BinaryOperator::Inequality
        | BinaryOperator::StrictEquality
        | BinaryOperator::StrictInequality
        | BinaryOperator::LessThan
        | BinaryOperator::LessEqualThan
        | BinaryOperator::GreaterThan
        | BinaryOperator::GreaterEqualThan
        | BinaryOperator::In
        | BinaryOperator::Instanceof => {
          Some(TsTypeDef::bool_with_repr("boolean"))
        }
        BinaryOperator::ShiftLeft
        | BinaryOperator::ShiftRight
        | BinaryOperator::ShiftRightZeroFill
        | BinaryOperator::Subtraction
        | BinaryOperator::Multiplication
        | BinaryOperator::Division
        | BinaryOperator::Remainder
        | BinaryOperator::BitwiseOR
        | BinaryOperator::BitwiseXOR
        | BinaryOperator::BitwiseAnd
        | BinaryOperator::Exponential => {
          Some(TsTypeDef::number_with_repr("number"))
        }
        BinaryOperator::Addition => None,
      }
    }
    Expression::LogicalExpression(_)
    | Expression::ThisExpression(_)
    | Expression::UnaryExpression(_)
    | Expression::AssignmentExpression(_)
    | Expression::StaticMemberExpression(_)
    | Expression::ComputedMemberExpression(_)
    | Expression::PrivateFieldExpression(_)
    | Expression::Super(_)
    | Expression::SequenceExpression(_)
    | Expression::Identifier(_)
    | Expression::TaggedTemplateExpression(_)
    | Expression::ClassExpression(_)
    | Expression::YieldExpression(_)
    | Expression::MetaProperty(_)
    | Expression::JSXElement(_)
    | Expression::JSXFragment(_)
    | Expression::TSInstantiationExpression(_)
    | Expression::ChainExpression(_)
    | Expression::ImportExpression(_)
    | Expression::PrivateInExpression(_)
    | Expression::V8IntrinsicExpression(_) => None,
  }
}

pub(crate) fn infer_simple_ts_type_from_init(
  module_info: &EsModuleInfo,
  init: Option<&Expression>,
  is_const: bool,
) -> Option<TsTypeDef> {
  if let Some(init_expr) = init {
    infer_ts_type_from_expr(module_info, init_expr, is_const)
  } else {
    None
  }
}

fn infer_ts_type_from_arr_lit(
  module_info: &EsModuleInfo,
  arr_lit: &ArrayExpression,
  is_const: bool,
) -> Option<TsTypeDef> {
  let mut defs = Vec::new();
  for elem in &arr_lit.elements {
    match elem {
      ArrayExpressionElement::SpreadElement(_) => {
        return Some(TsTypeDef {
          repr: "any[]".to_string(),
          kind: TsTypeDefKind::Array(Box::new(TsTypeDef::keyword("any"))),
        });
      }
      ArrayExpressionElement::Elision(_) => continue,
      _ => {
        let expr = elem.to_expression();
        if let Some(ts_type) =
          infer_ts_type_from_expr(module_info, expr, is_const)
        {
          if !defs.contains(&ts_type) {
            defs.push(ts_type);
          }
        } else {
          // it is not a trivial type that can be inferred an so will infer an
          // an any array.
          return Some(TsTypeDef {
            repr: "any[]".to_string(),
            kind: TsTypeDefKind::Array(Box::new(TsTypeDef::keyword("any"))),
          });
        }
      }
    }
  }
  match defs.len() {
    1 => Some(TsTypeDef {
      kind: TsTypeDefKind::Array(Box::new(defs[0].clone())),
      repr: String::new(),
    }),
    2.. => {
      let union = TsTypeDef {
        kind: TsTypeDefKind::Union(defs),
        repr: String::new(),
      };
      Some(TsTypeDef {
        kind: TsTypeDefKind::Array(Box::new(union)),
        repr: String::new(),
      })
    }
    _ => None,
  }
}

fn infer_ts_type_from_arrow_expr(
  module_info: &EsModuleInfo,
  expr: &ArrowFunctionExpression,
) -> Option<TsTypeDef> {
  Some(TsTypeDef {
    kind: TsTypeDefKind::FnOrConstructor(Box::new(
      TsFnOrConstructorDef::arrow_expr(module_info, expr),
    )),
    repr: String::new(),
  })
}

fn infer_ts_type_from_fn_expr(
  module_info: &EsModuleInfo,
  expr: &Function,
) -> Option<TsTypeDef> {
  Some(TsTypeDef {
    kind: TsTypeDefKind::FnOrConstructor(Box::new(
      TsFnOrConstructorDef::fn_expr(module_info, expr),
    )),
    repr: String::new(),
  })
}

fn is_const_assertion(ts_type: &TSType) -> bool {
  matches!(
    ts_type,
    TSType::TSTypeReference(type_ref)
      if matches!(&type_ref.type_name, TSTypeName::IdentifierReference(ident) if ident.name == "const")
  )
}

fn infer_ts_type_from_new_expr(
  module_info: &EsModuleInfo,
  new_expr: &NewExpression,
) -> Option<TsTypeDef> {
  match &new_expr.callee {
    Expression::Identifier(ident) => Some(TsTypeDef {
      repr: ident.name.to_string(),
      kind: TsTypeDefKind::TypeRef(TsTypeRefDef {
        type_params: new_expr.type_arguments.as_ref().map(|init| {
          maybe_type_param_instantiation_to_type_defs(module_info, Some(init))
        }),
        type_name: ident.name.to_string(),
        resolution: resolve_type_ref(module_info, ident),
      }),
    }),
    _ => None,
  }
}

fn infer_ts_type_from_call_expr(
  call_expr: &CallExpression,
) -> Option<TsTypeDef> {
  if let Expression::Identifier(ident) = &call_expr.callee {
    let sym = ident.name.to_string();
    match sym.as_str() {
      "Symbol" | "Number" | "String" | "BigInt" => Some(
        TsTypeDef::keyword_with_repr(&sym.to_ascii_lowercase(), &sym.clone()),
      ),
      "Date" => Some(TsTypeDef::string_with_repr(&sym)),
      "RegExp" => Some(TsTypeDef::regexp(sym)),
      _ => None,
    }
  } else {
    None
  }
}

fn infer_ts_type_from_obj(
  module_info: &EsModuleInfo,
  obj: &ObjectExpression,
) -> Option<TsTypeDef> {
  let (methods, properties) = infer_ts_type_from_obj_inner(module_info, obj);
  if methods.is_empty() && properties.is_empty() {
    None
  } else {
    Some(TsTypeDef::object(methods, properties))
  }
}

fn infer_ts_type_from_obj_inner(
  module_info: &EsModuleInfo,
  obj: &ObjectExpression,
) -> (Vec<MethodDef>, Vec<PropertyDef>) {
  let mut methods = Vec::<MethodDef>::new();
  let mut properties = Vec::<PropertyDef>::new();
  for obj_prop in &obj.properties {
    let Some(js_doc) = js_doc_for_range(module_info, obj_prop.span()) else {
      continue;
    };

    match obj_prop {
      ObjectPropertyKind::ObjectProperty(prop) => {
        if prop.shorthand {
          let name = prop
            .key
            .static_name()
            .map(|name| name.to_string())
            .unwrap_or_else(|| "[UNSUPPORTED]".to_string());
          properties.push(PropertyDef {
            name,
            js_doc,
            location: get_location(module_info, prop.span.start),
            params: vec![],
            readonly: false,
            computed: false,
            optional: false,
            ts_type: None,
            type_params: Box::new([]),
          });
        } else if prop.kind == PropertyKind::Get {
          let name = prop_name_to_string(module_info, &prop.key);
          let computed = prop.key.static_name().is_none();
          let return_type =
            if let Expression::FunctionExpression(func) = &prop.value {
              func.return_type.as_ref().map(|type_ann| {
                TsTypeDef::new(module_info, &type_ann.type_annotation)
              })
            } else {
              None
            };
          methods.push(MethodDef {
            name,
            js_doc,
            kind: MethodKind::Getter,
            location: get_location(module_info, prop.span.start),
            params: vec![],
            computed,
            optional: false,
            return_type,
            type_params: Box::new([]),
          });
        } else if prop.kind == PropertyKind::Set {
          let name = prop_name_to_string(module_info, &prop.key);
          let computed = prop.key.static_name().is_none();
          let params = if let Expression::FunctionExpression(func) = &prop.value
          {
            formal_params_to_param_defs(module_info, &func.params)
          } else {
            vec![]
          };
          methods.push(MethodDef {
            name,
            js_doc,
            kind: MethodKind::Setter,
            location: get_location(module_info, prop.span.start),
            params,
            computed,
            optional: false,
            return_type: None,
            type_params: Box::new([]),
          });
        } else if prop.method {
          let name = prop_name_to_string(module_info, &prop.key);
          let computed = prop.key.static_name().is_none();
          if let Expression::FunctionExpression(func) = &prop.value {
            let params = formal_params_to_param_defs(module_info, &func.params);
            let return_type = func.return_type.as_ref().map(|type_ann| {
              TsTypeDef::new(module_info, &type_ann.type_annotation)
            });
            let type_params = maybe_type_param_decl_to_type_param_defs(
              module_info,
              func.type_parameters.as_deref(),
            );
            methods.push(MethodDef {
              name,
              js_doc,
              kind: MethodKind::Method,
              location: get_location(module_info, prop.span.start),
              params,
              computed,
              optional: false,
              return_type,
              type_params,
            });
          }
        } else {
          properties.push(PropertyDef {
            name: prop_name_to_string(module_info, &prop.key),
            js_doc,
            location: get_location(module_info, prop.span.start),
            params: vec![],
            readonly: false,
            computed: prop.key.static_name().is_none(),
            optional: false,
            ts_type: infer_ts_type_from_expr(module_info, &prop.value, false),
            type_params: Box::new([]),
          });
        }
      }
      ObjectPropertyKind::SpreadProperty(spread) => {
        if let Expression::ObjectExpression(obj) = &spread.argument {
          let (spread_methods, spread_properties) =
            infer_ts_type_from_obj_inner(module_info, obj);
          methods.extend(spread_methods);
          properties.extend(spread_properties);
        }
      }
    }
  }
  (methods, properties)
}

fn infer_ts_type_from_tpl(
  module_info: &EsModuleInfo,
  tpl: &TemplateLiteral,
  is_const: bool,
) -> TsTypeDef {
  let exprs = tpl
    .expressions
    .iter()
    .map(|expr| infer_ts_type_from_expr(module_info, expr, is_const))
    .collect::<Option<Vec<_>>>();

  if let Some(exprs) = exprs {
    TsTypeDef::tpl_literal(exprs, &tpl.quasis)
  } else {
    TsTypeDef::string_with_repr("string")
  }
}

impl Display for TsTypeDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    match &self.kind {
      TsTypeDefKind::Array(array) => {
        if matches!(
          array.kind,
          TsTypeDefKind::Union(_) | TsTypeDefKind::Intersection(_)
        ) {
          write!(f, "({})[]", array)
        } else {
          write!(f, "{}[]", array)
        }
      }
      TsTypeDefKind::Conditional(conditional) => {
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
      TsTypeDefKind::Infer(infer) => {
        write!(f, "{} {}", colors::magenta("infer"), infer.type_param)
      }
      TsTypeDefKind::ImportType(import_type) => {
        write!(f, "import(\"{}\")", import_type.specifier)?;
        if let Some(qualifier) = &import_type.qualifier {
          write!(f, ".{}", qualifier)?;
        }
        if let Some(type_params) = &import_type.type_params {
          write!(f, "<{}>", SliceDisplayer::new(type_params, ", ", false))?;
        }
        Ok(())
      }
      TsTypeDefKind::FnOrConstructor(fn_or_constructor) => {
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
      TsTypeDefKind::IndexedAccess(indexed_access) => {
        write!(
          f,
          "{}[{}]",
          &*indexed_access.obj_type, &*indexed_access.index_type
        )
      }
      TsTypeDefKind::Intersection(intersection) => {
        write!(f, "{}", SliceDisplayer::new(intersection, " & ", false))
      }
      TsTypeDefKind::Mapped(mapped_type) => {
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
      TsTypeDefKind::Keyword(keyword) => {
        write!(f, "{}", colors::cyan(keyword))
      }
      TsTypeDefKind::Literal(literal) => match literal.kind {
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
            if let TsTypeDefKind::Literal(literal) = &ts_type.kind
              && literal.kind == LiteralDefKind::String
            {
              write!(f, "{}", colors::green(literal.string.as_ref().unwrap()))?;
              continue;
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
          write!(f, "{}n", colors::yellow(&literal.string.as_ref().unwrap()))
        }
      },
      TsTypeDefKind::Optional(optional) => {
        write!(f, "{}?", optional)
      }
      TsTypeDefKind::Parenthesized(parenthesized) => {
        write!(f, "({})", parenthesized)
      }
      TsTypeDefKind::Rest(rest) => write!(f, "...{}", rest),
      TsTypeDefKind::This => write!(f, "this"),
      TsTypeDefKind::Tuple(tuple) => {
        write!(f, "[{}]", SliceDisplayer::new(tuple, ", ", false))
      }
      TsTypeDefKind::TypeLiteral(type_literal) => {
        write!(
          f,
          "{{ {}{}{}{}}}",
          SliceDisplayer::new(&type_literal.call_signatures, "; ", true),
          SliceDisplayer::new(&type_literal.methods, "; ", true),
          SliceDisplayer::new(&type_literal.properties, "; ", true),
          SliceDisplayer::new(&type_literal.index_signatures, "; ", true),
        )
      }
      TsTypeDefKind::TypeOperator(type_operator) => {
        write!(f, "{} {}", type_operator.operator, &type_operator.ts_type)
      }
      TsTypeDefKind::TypeQuery(type_query) => {
        write!(f, "typeof {}", type_query)
      }
      TsTypeDefKind::TypeRef(type_ref) => {
        write!(f, "{}", colors::intense_blue(&type_ref.type_name))?;
        if let Some(type_params) = &type_ref.type_params {
          write!(f, "<{}>", SliceDisplayer::new(type_params, ", ", false))?;
        }
        Ok(())
      }
      TsTypeDefKind::Union(union) => {
        write!(f, "{}", SliceDisplayer::new(union, " | ", false))
      }
      TsTypeDefKind::TypePredicate(type_predicate) => {
        write!(f, "{}", type_predicate)
      }
      TsTypeDefKind::Unsupported => {
        write!(f, "{}", self.repr)
      }
    }
  }
}

pub(crate) fn maybe_type_param_instantiation_to_type_defs(
  module_info: &EsModuleInfo,
  maybe_type_param_instantiation: Option<&TSTypeParameterInstantiation>,
) -> Box<[TsTypeDef]> {
  if let Some(type_param_instantiation) = maybe_type_param_instantiation {
    type_param_instantiation
      .params
      .iter()
      .map(|type_param| TsTypeDef::new(module_info, type_param))
      .collect::<Box<[TsTypeDef]>>()
  } else {
    Box::new([])
  }
}

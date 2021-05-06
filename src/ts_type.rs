// Copyright 2020-2021 the Deno authors. All rights reserved. MIT license.
use crate::colors;
use crate::display::{display_readonly, SliceDisplayer};
use crate::interface::expr_to_name;
use crate::params::ts_fn_param_to_param_def;
use crate::ts_type_param::maybe_type_param_decl_to_type_param_defs;
use crate::ts_type_param::TsTypeParamDef;
use crate::ParamDef;
use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter, Result as FmtResult};
use swc_ecmascript::ast::{
  BigInt, Bool, CallExpr, Expr, ExprOrSuper, Lit, NewExpr, Number, Str, Tpl,
  TplElement, TsArrayType, TsConditionalType, TsExprWithTypeArgs,
  TsFnOrConstructorType, TsIndexedAccessType, TsKeywordType, TsLit, TsLitType,
  TsOptionalType, TsParenthesizedType, TsRestType, TsThisType, TsTupleType,
  TsType, TsTypeAnn, TsTypeLit, TsTypeOperator, TsTypeParamInstantiation,
  TsTypeQuery, TsTypeRef, TsUnionOrIntersectionType, VarDeclarator,
};

// pub enum TsType {
//  *      TsKeywordType(TsKeywordType),
//  *      TsThisType(TsThisType),
//  *      TsFnOrConstructorType(TsFnOrConstructorType),
//  *      TsTypeRef(TsTypeRef),
//  *      TsTypeQuery(TsTypeQuery),
//  *      TsTypeLit(TsTypeLit),
//  *      TsArrayType(TsArrayType),
//  *      TsTupleType(TsTupleType),
//  *      TsOptionalType(TsOptionalType),
//  *      TsRestType(TsRestType),
//  *      TsUnionOrIntersectionType(TsUnionOrIntersectionType),
//  *      TsConditionalType(TsConditionalType),
//  *      TsParenthesizedType(TsParenthesizedType),
//  *      TsTypeOperator(TsTypeOperator),
//  *      TsIndexedAccessType(TsIndexedAccessType),
//  *      TsLitType(TsLitType),
//     TsInferType(TsInferType),
//     TsMappedType(TsMappedType),
//     TsTypePredicate(TsTypePredicate),
//     TsImportType(TsImportType),
// }

impl From<&TsLitType> for TsTypeDef {
  fn from(other: &TsLitType) -> TsTypeDef {
    match &other.lit {
      TsLit::Number(num) => (TsTypeDef::number_literal(num)),
      TsLit::Str(str_) => (TsTypeDef::string_literal(str_)),
      TsLit::Tpl(tpl) => TsTypeDef::tpl_literal(&tpl.quasis),
      TsLit::Bool(bool_) => (TsTypeDef::bool_literal(bool_)),
      TsLit::BigInt(bigint_) => (TsTypeDef::bigint_literal(bigint_)),
    }
  }
}

impl From<&TsArrayType> for TsTypeDef {
  fn from(other: &TsArrayType) -> TsTypeDef {
    let ts_type_def: TsTypeDef = (&*other.elem_type).into();

    TsTypeDef {
      array: Some(Box::new(ts_type_def)),
      kind: Some(TsTypeDefKind::Array),
      ..Default::default()
    }
  }
}

impl From<&TsTupleType> for TsTypeDef {
  fn from(other: &TsTupleType) -> TsTypeDef {
    let mut type_defs = vec![];

    for type_box in &other.elem_types {
      let ts_type: &TsType = &type_box.ty;
      let def: TsTypeDef = ts_type.into();
      type_defs.push(def)
    }

    TsTypeDef {
      tuple: Some(type_defs),
      kind: Some(TsTypeDefKind::Tuple),
      ..Default::default()
    }
  }
}

impl From<&TsUnionOrIntersectionType> for TsTypeDef {
  fn from(other: &TsUnionOrIntersectionType) -> TsTypeDef {
    use swc_ecmascript::ast::TsUnionOrIntersectionType::*;

    match other {
      TsUnionType(union_type) => {
        let mut types_union = vec![];

        for type_box in &union_type.types {
          let ts_type: &TsType = &(*type_box);
          let def: TsTypeDef = ts_type.into();
          types_union.push(def);
        }

        TsTypeDef {
          union: Some(types_union),
          kind: Some(TsTypeDefKind::Union),
          ..Default::default()
        }
      }
      TsIntersectionType(intersection_type) => {
        let mut types_intersection = vec![];

        for type_box in &intersection_type.types {
          let ts_type: &TsType = &(*type_box);
          let def: TsTypeDef = ts_type.into();
          types_intersection.push(def);
        }

        TsTypeDef {
          intersection: Some(types_intersection),
          kind: Some(TsTypeDefKind::Intersection),
          ..Default::default()
        }
      }
    }
  }
}

impl From<&TsKeywordType> for TsTypeDef {
  fn from(other: &TsKeywordType) -> TsTypeDef {
    use swc_ecmascript::ast::TsKeywordTypeKind::*;

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
}

impl From<&TsTypeOperator> for TsTypeDef {
  fn from(other: &TsTypeOperator) -> TsTypeDef {
    let ts_type = (&*other.type_ann).into();
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
}

impl From<&TsParenthesizedType> for TsTypeDef {
  fn from(other: &TsParenthesizedType) -> TsTypeDef {
    let ts_type = (&*other.type_ann).into();

    TsTypeDef {
      parenthesized: Some(Box::new(ts_type)),
      kind: Some(TsTypeDefKind::Parenthesized),
      ..Default::default()
    }
  }
}

impl From<&TsRestType> for TsTypeDef {
  fn from(other: &TsRestType) -> TsTypeDef {
    let ts_type = (&*other.type_ann).into();

    TsTypeDef {
      rest: Some(Box::new(ts_type)),
      kind: Some(TsTypeDefKind::Rest),
      ..Default::default()
    }
  }
}

impl From<&TsOptionalType> for TsTypeDef {
  fn from(other: &TsOptionalType) -> TsTypeDef {
    let ts_type = (&*other.type_ann).into();

    TsTypeDef {
      optional: Some(Box::new(ts_type)),
      kind: Some(TsTypeDefKind::Optional),
      ..Default::default()
    }
  }
}

impl From<&TsThisType> for TsTypeDef {
  fn from(_: &TsThisType) -> TsTypeDef {
    TsTypeDef {
      repr: "this".to_string(),
      this: Some(true),
      kind: Some(TsTypeDefKind::This),
      ..Default::default()
    }
  }
}

pub fn ts_entity_name_to_name(
  entity_name: &swc_ecmascript::ast::TsEntityName,
) -> String {
  use swc_ecmascript::ast::TsEntityName::*;

  match entity_name {
    Ident(ident) => ident.sym.to_string(),
    TsQualifiedName(ts_qualified_name) => {
      let left = ts_entity_name_to_name(&ts_qualified_name.left);
      let right = ts_qualified_name.right.sym.to_string();
      format!("{}.{}", left, right)
    }
  }
}

impl From<&TsTypeQuery> for TsTypeDef {
  fn from(other: &TsTypeQuery) -> TsTypeDef {
    use swc_ecmascript::ast::TsTypeQueryExpr::*;

    let type_name = match &other.expr_name {
      TsEntityName(entity_name) => ts_entity_name_to_name(&*entity_name),
      Import(import_type) => import_type.arg.value.to_string(),
    };

    TsTypeDef {
      repr: type_name.to_string(),
      type_query: Some(type_name),
      kind: Some(TsTypeDefKind::TypeQuery),
      ..Default::default()
    }
  }
}

impl From<&TsTypeRef> for TsTypeDef {
  fn from(other: &TsTypeRef) -> TsTypeDef {
    let type_name = ts_entity_name_to_name(&other.type_name);

    let type_params = if let Some(type_params_inst) = &other.type_params {
      let mut ts_type_defs = vec![];

      for type_box in &type_params_inst.params {
        let ts_type: &TsType = &(*type_box);
        let def: TsTypeDef = ts_type.into();
        ts_type_defs.push(def);
      }

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
}

impl From<&TsExprWithTypeArgs> for TsTypeDef {
  fn from(other: &TsExprWithTypeArgs) -> TsTypeDef {
    let type_name = ts_entity_name_to_name(&other.expr);

    let type_params = if let Some(type_params_inst) = &other.type_args {
      let mut ts_type_defs = vec![];

      for type_box in &type_params_inst.params {
        let ts_type: &TsType = &(*type_box);
        let def: TsTypeDef = ts_type.into();
        ts_type_defs.push(def);
      }

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
}

impl From<&TsIndexedAccessType> for TsTypeDef {
  fn from(other: &TsIndexedAccessType) -> TsTypeDef {
    let indexed_access_def = TsIndexedAccessDef {
      readonly: other.readonly,
      obj_type: Box::new((&*other.obj_type).into()),
      index_type: Box::new((&*other.index_type).into()),
    };

    TsTypeDef {
      indexed_access: Some(indexed_access_def),
      kind: Some(TsTypeDefKind::IndexedAccess),
      ..Default::default()
    }
  }
}

impl From<&TsTypeLit> for TsTypeDef {
  fn from(other: &TsTypeLit) -> TsTypeDef {
    let mut methods = vec![];
    let mut properties = vec![];
    let mut call_signatures = vec![];
    let mut index_signatures = vec![];

    for type_element in &other.members {
      use swc_ecmascript::ast::TsTypeElement::*;

      match &type_element {
        TsMethodSignature(ts_method_sig) => {
          let mut params = vec![];

          for param in &ts_method_sig.params {
            let param_def = ts_fn_param_to_param_def(param, None);
            params.push(param_def);
          }

          let maybe_return_type = ts_method_sig
            .type_ann
            .as_ref()
            .map(|rt| (&*rt.type_ann).into());

          let type_params = maybe_type_param_decl_to_type_param_defs(
            ts_method_sig.type_params.as_ref(),
          );
          let name = expr_to_name(&*ts_method_sig.key);
          let method_def = LiteralMethodDef {
            name,
            params,
            return_type: maybe_return_type,
            type_params,
          };
          methods.push(method_def);
        }
        TsGetterSignature(ts_getter_sig) => {
          let maybe_return_type = ts_getter_sig
            .type_ann
            .as_ref()
            .map(|rt| (&*rt.type_ann).into());

          let name = expr_to_name(&*ts_getter_sig.key);
          let method_def = LiteralMethodDef {
            name,
            params: vec![],
            return_type: maybe_return_type,
            type_params: vec![],
          };
          methods.push(method_def);
        }
        TsSetterSignature(ts_setter_sig) => {
          let name = expr_to_name(&*ts_setter_sig.key);
          let method_def = LiteralMethodDef {
            name,
            params: vec![],
            return_type: None,
            type_params: vec![],
          };
          methods.push(method_def);
        }
        TsPropertySignature(ts_prop_sig) => {
          let name = expr_to_name(&*ts_prop_sig.key);

          let mut params = vec![];

          for param in &ts_prop_sig.params {
            let param_def = ts_fn_param_to_param_def(param, None);
            params.push(param_def);
          }

          let ts_type = ts_prop_sig
            .type_ann
            .as_ref()
            .map(|rt| (&*rt.type_ann).into());

          let type_params = maybe_type_param_decl_to_type_param_defs(
            ts_prop_sig.type_params.as_ref(),
          );
          let prop_def = LiteralPropertyDef {
            name,
            params,
            ts_type,
            computed: ts_prop_sig.computed,
            optional: ts_prop_sig.optional,
            type_params,
          };
          properties.push(prop_def);
        }
        TsCallSignatureDecl(ts_call_sig) => {
          let mut params = vec![];
          for param in &ts_call_sig.params {
            let param_def = ts_fn_param_to_param_def(param, None);
            params.push(param_def);
          }

          let ts_type = ts_call_sig
            .type_ann
            .as_ref()
            .map(|rt| (&*rt.type_ann).into());

          let type_params = maybe_type_param_decl_to_type_param_defs(
            ts_call_sig.type_params.as_ref(),
          );

          let call_sig_def = LiteralCallSignatureDef {
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

          let index_sig_def = LiteralIndexSignatureDef {
            readonly: ts_index_sig.readonly,
            params,
            ts_type,
          };
          index_signatures.push(index_sig_def);
        }
        // TODO:
        TsConstructSignatureDecl(_) => {}
      }
    }

    let type_literal = TsTypeLiteralDef {
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
}

impl From<&TsConditionalType> for TsTypeDef {
  fn from(other: &TsConditionalType) -> TsTypeDef {
    let conditional_type_def = TsConditionalDef {
      check_type: Box::new((&*other.check_type).into()),
      extends_type: Box::new((&*other.extends_type).into()),
      true_type: Box::new((&*other.true_type).into()),
      false_type: Box::new((&*other.false_type).into()),
    };

    TsTypeDef {
      kind: Some(TsTypeDefKind::Conditional),
      conditional_type: Some(conditional_type_def),
      ..Default::default()
    }
  }
}

impl From<&TsFnOrConstructorType> for TsTypeDef {
  fn from(other: &TsFnOrConstructorType) -> TsTypeDef {
    use swc_ecmascript::ast::TsFnOrConstructorType::*;

    let fn_def = match other {
      TsFnType(ts_fn_type) => {
        let mut params = vec![];

        for param in &ts_fn_type.params {
          let param_def = ts_fn_param_to_param_def(param, None);
          params.push(param_def);
        }

        let type_params = maybe_type_param_decl_to_type_param_defs(
          ts_fn_type.type_params.as_ref(),
        );

        TsFnOrConstructorDef {
          constructor: false,
          ts_type: ts_type_ann_to_def(&ts_fn_type.type_ann),
          params,
          type_params,
        }
      }
      TsConstructorType(ctor_type) => {
        let mut params = vec![];

        for param in &ctor_type.params {
          let param_def = ts_fn_param_to_param_def(param, None);
          params.push(param_def);
        }

        let type_params = maybe_type_param_decl_to_type_param_defs(
          ctor_type.type_params.as_ref(),
        );
        TsFnOrConstructorDef {
          constructor: true,
          ts_type: ts_type_ann_to_def(&ctor_type.type_ann),
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
}

impl From<&TsType> for TsTypeDef {
  fn from(other: &TsType) -> TsTypeDef {
    use swc_ecmascript::ast::TsType::*;

    match other {
      TsKeywordType(ref keyword_type) => keyword_type.into(),
      TsLitType(ref lit_type) => lit_type.into(),
      TsTypeRef(ref type_ref) => type_ref.into(),
      TsUnionOrIntersectionType(union_or_inter) => union_or_inter.into(),
      TsArrayType(array_type) => array_type.into(),
      TsTupleType(tuple_type) => tuple_type.into(),
      TsTypeOperator(type_op_type) => type_op_type.into(),
      TsParenthesizedType(paren_type) => paren_type.into(),
      TsRestType(rest_type) => rest_type.into(),
      TsOptionalType(optional_type) => optional_type.into(),
      TsTypeQuery(type_query) => type_query.into(),
      TsThisType(this_type) => this_type.into(),
      TsFnOrConstructorType(fn_or_con_type) => fn_or_con_type.into(),
      TsConditionalType(conditional_type) => conditional_type.into(),
      TsIndexedAccessType(indexed_access_type) => indexed_access_type.into(),
      TsTypeLit(type_literal) => type_literal.into(),
      _ => TsTypeDef {
        repr: "<UNIMPLEMENTED>".to_string(),
        ..Default::default()
      },
    }
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct TsTypeRefDef {
  pub type_params: Option<Vec<TsTypeDef>>,
  pub type_name: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub enum LiteralDefKind {
  Number,
  String,
  Boolean,
  BigInt,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LiteralDef {
  pub kind: LiteralDefKind,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub number: Option<f64>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub string: Option<String>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub boolean: Option<bool>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct TsTypeOperatorDef {
  pub operator: String,
  pub ts_type: TsTypeDef,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct TsFnOrConstructorDef {
  pub constructor: bool,
  pub ts_type: TsTypeDef,
  pub params: Vec<ParamDef>,
  pub type_params: Vec<TsTypeParamDef>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct TsConditionalDef {
  pub check_type: Box<TsTypeDef>,
  pub extends_type: Box<TsTypeDef>,
  pub true_type: Box<TsTypeDef>,
  pub false_type: Box<TsTypeDef>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct TsIndexedAccessDef {
  pub readonly: bool,
  pub obj_type: Box<TsTypeDef>,
  pub index_type: Box<TsTypeDef>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LiteralMethodDef {
  pub name: String,
  pub params: Vec<ParamDef>,
  pub return_type: Option<TsTypeDef>,
  pub type_params: Vec<TsTypeParamDef>,
}

impl Display for LiteralMethodDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{}({})",
      self.name,
      SliceDisplayer::new(&self.params, ", ", false)
    )?;
    if let Some(return_type) = &self.return_type {
      write!(f, ": {}", return_type)?;
    }
    Ok(())
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LiteralPropertyDef {
  pub name: String,
  pub params: Vec<ParamDef>,
  pub computed: bool,
  pub optional: bool,
  pub ts_type: Option<TsTypeDef>,
  pub type_params: Vec<TsTypeParamDef>,
}

impl Display for LiteralPropertyDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{}", self.name)?;
    if let Some(ts_type) = &self.ts_type {
      write!(f, ": {}", ts_type)?;
    }
    Ok(())
  }
}
#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LiteralCallSignatureDef {
  pub params: Vec<ParamDef>,
  pub ts_type: Option<TsTypeDef>,
  pub type_params: Vec<TsTypeParamDef>,
}

impl Display for LiteralCallSignatureDef {
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
pub struct LiteralIndexSignatureDef {
  pub readonly: bool,
  pub params: Vec<ParamDef>,
  pub ts_type: Option<TsTypeDef>,
}

impl Display for LiteralIndexSignatureDef {
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
pub struct TsTypeLiteralDef {
  pub methods: Vec<LiteralMethodDef>,
  pub properties: Vec<LiteralPropertyDef>,
  pub call_signatures: Vec<LiteralCallSignatureDef>,
  pub index_signatures: Vec<LiteralIndexSignatureDef>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
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
  IndexedAccess,
  TypeLiteral,
}

#[derive(Debug, Default, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct TsTypeDef {
  pub repr: String,

  pub kind: Option<TsTypeDefKind>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub keyword: Option<String>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub literal: Option<LiteralDef>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_ref: Option<TsTypeRefDef>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub union: Option<Vec<TsTypeDef>>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub intersection: Option<Vec<TsTypeDef>>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub array: Option<Box<TsTypeDef>>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub tuple: Option<Vec<TsTypeDef>>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_operator: Option<Box<TsTypeOperatorDef>>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub parenthesized: Option<Box<TsTypeDef>>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub rest: Option<Box<TsTypeDef>>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub optional: Option<Box<TsTypeDef>>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_query: Option<String>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub this: Option<bool>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub fn_or_constructor: Option<Box<TsFnOrConstructorDef>>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub conditional_type: Option<TsConditionalDef>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub indexed_access: Option<TsIndexedAccessDef>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_literal: Option<TsTypeLiteralDef>,
}

impl TsTypeDef {
  pub fn number_literal(num: &Number) -> TsTypeDef {
    let repr = format!("{}", num.value);
    let lit = LiteralDef {
      kind: LiteralDefKind::Number,
      number: Some(num.value),
      string: None,
      boolean: None,
    };
    Self::literal(repr, lit)
  }

  pub fn string_literal(str_: &Str) -> TsTypeDef {
    let repr = str_.value.to_string();
    let lit = LiteralDef {
      kind: LiteralDefKind::String,
      number: None,
      string: Some(str_.value.to_string()),
      boolean: None,
    };
    Self::literal(repr, lit)
  }

  pub fn tpl_literal(quasis: &[TplElement]) -> TsTypeDef {
    // A template literal in a type is not allowed to have
    // expressions, so there will only be one quasi.
    let quasi = quasis.get(0).expect("Expected tpl to have a quasi.");
    let text = quasi.raw.value.to_string();
    let repr = text.clone();
    let lit = LiteralDef {
      kind: LiteralDefKind::String, // semantically the same
      number: None,
      string: Some(text),
      boolean: None,
    };
    Self::literal(repr, lit)
  }

  pub fn bool_literal(bool_: &Bool) -> TsTypeDef {
    let repr = bool_.value.to_string();
    let lit = LiteralDef {
      kind: LiteralDefKind::Boolean,
      number: None,
      string: None,
      boolean: Some(bool_.value),
    };
    Self::literal(repr, lit)
  }

  pub fn bigint_literal(bigint_: &BigInt) -> TsTypeDef {
    let repr = bigint_.value.to_string();
    let lit = LiteralDef {
      kind: LiteralDefKind::BigInt,
      number: None,
      string: Some(bigint_.value.to_string()),
      boolean: None,
    };
    Self::literal(repr, lit)
  }

  pub fn regexp(repr: String) -> TsTypeDef {
    TsTypeDef {
      repr,
      kind: Some(TsTypeDefKind::TypeRef),
      type_ref: Some(TsTypeRefDef {
        type_params: None,
        type_name: "RegExp".to_string(),
      }),
      ..Default::default()
    }
  }

  pub fn keyword(keyword_str: &str) -> TsTypeDef {
    Self::keyword_with_repr(keyword_str, keyword_str)
  }

  pub fn number_with_repr(repr: &str) -> TsTypeDef {
    Self::keyword_with_repr("number", repr)
  }

  pub fn string_with_repr(repr: &str) -> TsTypeDef {
    Self::keyword_with_repr("string", repr)
  }

  pub fn bool_with_repr(repr: &str) -> TsTypeDef {
    Self::keyword_with_repr("boolean", repr)
  }

  pub fn bigint_with_repr(repr: &str) -> TsTypeDef {
    Self::keyword_with_repr("bigint", repr)
  }

  pub fn keyword_with_repr(keyword_str: &str, repr: &str) -> TsTypeDef {
    TsTypeDef {
      repr: repr.to_string(),
      kind: Some(TsTypeDefKind::Keyword),
      keyword: Some(keyword_str.to_string()),
      ..Default::default()
    }
  }

  fn literal(repr: String, lit: LiteralDef) -> TsTypeDef {
    TsTypeDef {
      repr,
      kind: Some(TsTypeDefKind::Literal),
      literal: Some(lit),
      ..Default::default()
    }
  }
}

pub fn ts_type_ann_to_def(type_ann: &TsTypeAnn) -> TsTypeDef {
  use swc_ecmascript::ast::TsType::*;

  match &*type_ann.type_ann {
    TsKeywordType(keyword_type) => keyword_type.into(),
    TsLitType(lit_type) => lit_type.into(),
    TsTypeRef(type_ref) => type_ref.into(),
    TsUnionOrIntersectionType(union_or_inter) => union_or_inter.into(),
    TsArrayType(array_type) => array_type.into(),
    TsTupleType(tuple_type) => tuple_type.into(),
    TsTypeOperator(type_op_type) => type_op_type.into(),
    TsParenthesizedType(paren_type) => paren_type.into(),
    TsRestType(rest_type) => rest_type.into(),
    TsOptionalType(optional_type) => optional_type.into(),
    TsTypeQuery(type_query) => type_query.into(),
    TsThisType(this_type) => this_type.into(),
    TsFnOrConstructorType(fn_or_con_type) => fn_or_con_type.into(),
    TsConditionalType(conditional_type) => conditional_type.into(),
    TsIndexedAccessType(indexed_access_type) => indexed_access_type.into(),
    TsTypeLit(type_literal) => type_literal.into(),
    _ => TsTypeDef {
      repr: "<TODO>".to_string(),
      ..Default::default()
    },
  }
}

pub fn infer_simple_ts_type_from_expr(
  expr: &Expr,
  is_const: bool,
) -> Option<TsTypeDef> {
  match expr {
    Expr::Lit(lit) => {
      // e.g.) const n = 100;
      infer_ts_type_from_lit(&lit, is_const)
    }
    Expr::New(expr) => {
      // e.g.) const d = new Date()
      infer_ts_type_from_new_expr(expr)
    }
    Expr::Tpl(tpl) => {
      // e.g.) const s = `hello`;
      Some(infer_ts_type_from_tpl(tpl, is_const))
    }
    Expr::Call(expr) => {
      // e.g.) const value = Number(123);
      infer_ts_type_from_call_expr(expr)
    }
    _ => None,
  }
}

pub fn infer_simple_ts_type_from_var_decl(
  decl: &VarDeclarator,
  is_const: bool,
) -> Option<TsTypeDef> {
  if let Some(init_expr) = &decl.init {
    infer_simple_ts_type_from_expr(init_expr.as_ref(), is_const)
  } else {
    None
  }
}

fn infer_ts_type_from_lit(lit: &Lit, is_const: bool) -> Option<TsTypeDef> {
  match lit {
    Lit::Num(num) => {
      if is_const {
        Some(TsTypeDef::number_literal(num))
      } else {
        let repr = format!("{}", num.value);
        Some(TsTypeDef::number_with_repr(&repr))
      }
    }
    Lit::Str(str_) => {
      if is_const {
        Some(TsTypeDef::string_literal(str_))
      } else {
        let repr = str_.value.to_string();
        Some(TsTypeDef::string_with_repr(&repr))
      }
    }
    Lit::Bool(bool_) => {
      if is_const {
        Some(TsTypeDef::bool_literal(bool_))
      } else {
        let repr = bool_.value.to_string();
        Some(TsTypeDef::bool_with_repr(&repr))
      }
    }
    Lit::BigInt(bigint_) => {
      if is_const {
        Some(TsTypeDef::bigint_literal(bigint_))
      } else {
        let repr = bigint_.value.to_string();
        Some(TsTypeDef::bigint_with_repr(&repr))
      }
    }
    Lit::Regex(regex) => Some(TsTypeDef::regexp(regex.exp.to_string())),
    _ => None,
  }
}

fn infer_ts_type_from_new_expr(new_expr: &NewExpr) -> Option<TsTypeDef> {
  match new_expr.callee.as_ref() {
    Expr::Ident(ident) => Some(TsTypeDef {
      repr: ident.sym.to_string(),
      kind: Some(TsTypeDefKind::TypeRef),
      type_ref: Some(TsTypeRefDef {
        type_params: None,
        type_name: ident.sym.to_string(),
      }),
      ..Default::default()
    }),
    _ => None,
  }
}

fn infer_ts_type_from_call_expr(call_expr: &CallExpr) -> Option<TsTypeDef> {
  match &call_expr.callee {
    ExprOrSuper::Expr(expr) => {
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

fn infer_ts_type_from_tpl(tpl: &Tpl, is_const: bool) -> TsTypeDef {
  if tpl.quasis.len() == 1 && is_const {
    // If no placeholder is present, the type can be inferred.
    //   e.g.) const tpl = `Hello world!`; // tpl has the type of `"Hello world!"`.
    TsTypeDef::tpl_literal(&tpl.quasis)
  } else {
    // If placeholders are present, the type cannot be inferred.
    //   e.g.) const tpl = `Hello ${name}!`; // tpl has the type of `string`.
    TsTypeDef::string_with_repr("string")
  }
}

impl Display for TsTypeDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    if self.kind.is_none() {
      return write!(f, "{}", colors::red("<UNIMPLEMENTED>"));
    }

    let kind = self.kind.as_ref().unwrap();
    match kind {
      TsTypeDefKind::Array => write!(f, "{}[]", &*self.array.as_ref().unwrap()),
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
        write!(f, "{}", SliceDisplayer::new(&intersection, " & ", false))
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
          LiteralDefKind::Number => write!(
            f,
            "{}",
            colors::yellow(&literal.number.unwrap().to_string())
          ),
          LiteralDefKind::BigInt => write!(
            f,
            "{}",
            colors::yellow(&literal.string.as_ref().unwrap().to_string())
          ),
        }
      }
      TsTypeDefKind::Optional => {
        write!(f, "{}?", &*self.optional.as_ref().unwrap())
      }
      TsTypeDefKind::Parenthesized => {
        write!(f, "({})", &*self.parenthesized.as_ref().unwrap())
      }
      TsTypeDefKind::Rest => write!(f, "...{}", &*self.rest.as_ref().unwrap()),
      TsTypeDefKind::This => write!(f, "this"),
      TsTypeDefKind::Tuple => {
        let tuple = self.tuple.as_ref().unwrap();
        write!(f, "[{}]", SliceDisplayer::new(&tuple, ", ", false))
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
    }
  }
}

pub fn maybe_type_param_instantiation_to_type_defs(
  maybe_type_param_instantiation: Option<&TsTypeParamInstantiation>,
) -> Vec<TsTypeDef> {
  if let Some(type_param_instantiation) = maybe_type_param_instantiation {
    type_param_instantiation
      .params
      .iter()
      .map(|type_param| type_param.as_ref().into())
      .collect::<Vec<TsTypeDef>>()
  } else {
    vec![]
  }
}

// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use deno_ast::SourceRangedForSpanned;
use deno_graph::symbols::EsModuleInfo;
use serde::Deserialize;
use serde::Serialize;

use crate::Location;
use crate::js_doc::JsDoc;
use crate::ts_type::TsTypeDef;
use crate::ts_type::infer_ts_type_from_expr;
use crate::util::swc::get_location;
use crate::util::swc::js_doc_for_range;

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct EnumMemberDef {
  pub name: String,
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub init: Option<TsTypeDef>,
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,
  pub location: Location,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct EnumDef {
  pub members: Vec<EnumMemberDef>,
}

pub fn get_doc_for_ts_enum_decl(
  module_info: &EsModuleInfo,
  enum_decl: &deno_ast::swc::ast::TsEnumDecl,
) -> (String, EnumDef) {
  let enum_name = enum_decl.id.sym.to_string();
  let mut members = vec![];

  for enum_member in &enum_decl.members {
    use deno_ast::swc::ast::TsEnumMemberId::*;

    if let Some(js_doc) = js_doc_for_range(module_info, &enum_member.range()) {
      let name = match &enum_member.id {
        Ident(ident) => ident.sym.to_string(),
        Str(str_) => str_.value.to_string_lossy().into_owned(),
      };
      let init = if let Some(expr) = &enum_member.init {
        infer_ts_type_from_expr(module_info, expr, true)
      } else {
        None
      };

      let member_def = EnumMemberDef {
        name,
        init,
        js_doc,
        location: get_location(module_info, enum_member.start()),
      };
      members.push(member_def);
    }
  }

  let enum_def = EnumDef { members };

  (enum_name, enum_def)
}

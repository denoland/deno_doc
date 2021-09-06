// Copyright 2020-2021 the Deno authors. All rights reserved. MIT license.

use deno_ast::ParsedSource;
use serde::Deserialize;
use serde::Serialize;

use crate::js_doc::JsDoc;
use crate::swc_util::js_doc_for_span;

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct EnumMemberDef {
  pub name: String,
  #[serde(skip_serializing_if = "JsDoc::is_empty")]
  pub js_doc: JsDoc,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct EnumDef {
  pub members: Vec<EnumMemberDef>,
}

pub fn get_doc_for_ts_enum_decl(
  parsed_source: &ParsedSource,
  enum_decl: &deno_ast::swc::ast::TsEnumDecl,
) -> (String, EnumDef) {
  let enum_name = enum_decl.id.sym.to_string();
  let mut members = vec![];

  for enum_member in &enum_decl.members {
    use deno_ast::swc::ast::TsEnumMemberId::*;

    let member_js_doc = js_doc_for_span(parsed_source, &enum_member.span);

    let member_name = match &enum_member.id {
      Ident(ident) => ident.sym.to_string(),
      Str(str_) => str_.value.to_string(),
    };

    let member_def = EnumMemberDef {
      name: member_name,
      js_doc: member_js_doc,
    };
    members.push(member_def);
  }

  let enum_def = EnumDef { members };

  (enum_name, enum_def)
}

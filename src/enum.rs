// Copyright 2020-2021 the Deno authors. All rights reserved. MIT license.

use serde::Deserialize;
use serde::Serialize;

use crate::js_doc::JsDoc;
use crate::parser::DocParser;

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
  doc_parser: &DocParser,
  enum_decl: &swc_ecmascript::ast::TsEnumDecl,
) -> (String, EnumDef) {
  let enum_name = enum_decl.id.sym.to_string();
  let mut members = vec![];

  for enum_member in &enum_decl.members {
    use swc_ecmascript::ast::TsEnumMemberId::*;

    let member_js_doc = doc_parser.js_doc_for_span(enum_member.span);

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

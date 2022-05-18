// Copyright 2020-2022 the Deno authors. All rights reserved. MIT license.

use deno_ast::ParsedSource;
use deno_ast::SwcSourceRanged;
use serde::{Deserialize, Serialize};

use crate::node::DeclarationKind;
use crate::node::DocNode;
use crate::parser::DocParser;
use crate::swc_util::get_location;
use crate::swc_util::js_doc_for_range;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct NamespaceDef {
  pub elements: Vec<DocNode>,
}

pub fn get_doc_for_ts_namespace_decl(
  doc_parser: &DocParser,
  parsed_source: &ParsedSource,
  ts_namespace_decl: &deno_ast::swc::ast::TsNamespaceDecl,
) -> DocNode {
  let js_doc = js_doc_for_range(parsed_source, &ts_namespace_decl.range());
  let location = get_location(parsed_source, ts_namespace_decl.start());
  let namespace_name = ts_namespace_decl.id.sym.to_string();

  use deno_ast::swc::ast::TsNamespaceBody::*;

  let elements = match &*ts_namespace_decl.body {
    TsModuleBlock(ts_module_block) => doc_parser.get_doc_nodes_for_module_body(
      parsed_source,
      ts_module_block.body.clone(),
    ),
    TsNamespaceDecl(ts_namespace_decl) => {
      vec![get_doc_for_ts_namespace_decl(
        doc_parser,
        parsed_source,
        ts_namespace_decl,
      )]
    }
  };

  let ns_def = NamespaceDef { elements };

  DocNode::namespace(
    namespace_name,
    location,
    DeclarationKind::Declare,
    js_doc,
    ns_def,
  )
}

pub fn get_doc_for_ts_module(
  doc_parser: &DocParser,
  parsed_source: &ParsedSource,
  ts_module_decl: &deno_ast::swc::ast::TsModuleDecl,
) -> (String, NamespaceDef) {
  use deno_ast::swc::ast::TsModuleName;
  let namespace_name = match &ts_module_decl.id {
    TsModuleName::Ident(ident) => ident.sym.to_string(),
    TsModuleName::Str(str_) => str_.value.to_string(),
  };

  let elements = if let Some(body) = &ts_module_decl.body {
    use deno_ast::swc::ast::TsNamespaceBody::*;

    match &body {
      TsModuleBlock(ts_module_block) => doc_parser
        .get_doc_nodes_for_module_body(
          parsed_source,
          ts_module_block.body.clone(),
        ),
      TsNamespaceDecl(ts_namespace_decl) => {
        vec![get_doc_for_ts_namespace_decl(
          doc_parser,
          parsed_source,
          ts_namespace_decl,
        )]
      }
    }
  } else {
    vec![]
  };

  let ns_def = NamespaceDef { elements };

  (namespace_name, ns_def)
}

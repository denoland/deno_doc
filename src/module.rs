// Copyright 2020-2021 the Deno authors. All rights reserved. MIT license.

use deno_ast::swc::common::Spanned;
use deno_ast::ParsedSource;

use crate::swc_util::get_location;
use crate::swc_util::js_doc_for_span;
use crate::DocNode;
use crate::DocParser;

pub fn get_doc_node_for_export_decl(
  doc_parser: &DocParser,
  parsed_source: &ParsedSource,
  export_decl: &deno_ast::swc::ast::ExportDecl,
) -> DocNode {
  let export_span = export_decl.span();
  use deno_ast::swc::ast::Decl;

  let js_doc = js_doc_for_span(parsed_source, &export_span);
  let location = get_location(parsed_source, export_span.lo());

  match &export_decl.decl {
    Decl::Class(class_decl) => {
      let (name, class_def) =
        super::class::get_doc_for_class_decl(parsed_source, class_decl);
      DocNode::class(name, location, js_doc, class_def)
    }
    Decl::Fn(fn_decl) => {
      let (name, function_def) =
        super::function::get_doc_for_fn_decl(parsed_source, fn_decl);
      DocNode::function(name, location, js_doc, function_def)
    }
    Decl::Var(var_decl) => {
      let (name, var_def) = super::variable::get_doc_for_var_decl(var_decl);
      DocNode::variable(name, location, js_doc, var_def)
    }
    Decl::TsInterface(ts_interface_decl) => {
      let (name, interface_def) =
        super::interface::get_doc_for_ts_interface_decl(
          parsed_source,
          ts_interface_decl,
        );
      DocNode::interface(name, location, js_doc, interface_def)
    }
    Decl::TsTypeAlias(ts_type_alias) => {
      let (name, type_alias_def) =
        super::type_alias::get_doc_for_ts_type_alias_decl(
          parsed_source,
          ts_type_alias,
        );
      DocNode::type_alias(name, location, js_doc, type_alias_def)
    }
    Decl::TsEnum(ts_enum) => {
      let (name, enum_def) =
        super::r#enum::get_doc_for_ts_enum_decl(parsed_source, ts_enum);
      DocNode::r#enum(name, location, js_doc, enum_def)
    }
    Decl::TsModule(ts_module) => {
      let (name, namespace_def) = super::namespace::get_doc_for_ts_module(
        doc_parser,
        parsed_source,
        ts_module,
      );
      DocNode::namespace(name, location, js_doc, namespace_def)
    }
  }
}

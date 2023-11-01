// Copyright 2020-2022 the Deno authors. All rights reserved. MIT license.

use deno_ast::swc::ast::ModuleExportName;
use deno_ast::swc::common::comments::Comment;
use deno_ast::swc::common::comments::CommentKind;
use deno_ast::ParsedSource;
use deno_ast::SourcePos;
use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;
use deno_ast::SourceTextInfo;
use regex::Regex;

use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;
use crate::node::Location;

lazy_static! {
  static ref JS_DOC_RE: Regex = Regex::new(r"\s*\* ?").unwrap();
}

pub(crate) fn is_false(b: &bool) -> bool {
  !b
}

fn parse_js_doc(js_doc_comment: &Comment) -> JsDoc {
  let txt = js_doc_comment
    .text
    .split('\n')
    .map(|line| JS_DOC_RE.replace(line, "").to_string())
    .collect::<Vec<String>>()
    .join("\n")
    .trim()
    .to_string();
  txt.into()
}

pub(crate) fn js_doc_for_range_include_ignore(
  parsed_source: &ParsedSource,
  range: &SourceRange,
) -> JsDoc {
  let Some(comments) = parsed_source.comments().get_leading(range.start) else {
    return JsDoc::default();
  };
  if let Some(js_doc_comment) = comments.iter().rev().find(|comment| {
    comment.kind == CommentKind::Block && comment.text.starts_with('*')
  }) {
    parse_js_doc(js_doc_comment)
  } else {
    JsDoc::default()
  }
}

pub(crate) fn js_doc_for_range(
  parsed_source: &ParsedSource,
  range: &SourceRange,
) -> Option<JsDoc> {
  let js_doc = js_doc_for_range_include_ignore(parsed_source, range);
  if js_doc.tags.contains(&JsDocTag::Ignore) {
    None
  } else {
    Some(js_doc)
  }
}

/// Inspects leading comments in the source and returns the first JSDoc comment
/// with a `@module` tag along with its associated range, otherwise returns
/// `None`.
pub(crate) fn module_js_doc_for_source(
  parsed_source: &ParsedSource,
) -> Option<Option<(JsDoc, SourceRange)>> {
  let comments = parsed_source.get_leading_comments();
  if let Some(js_doc_comment) = comments.iter().find(|comment| {
    comment.kind == CommentKind::Block && comment.text.starts_with('*')
  }) {
    let js_doc = parse_js_doc(js_doc_comment);
    if js_doc.tags.contains(&JsDocTag::Module) {
      if js_doc.tags.contains(&JsDocTag::Ignore) {
        return Some(None);
      }
      return Some(Some((js_doc, js_doc_comment.range())));
    }
  }
  None
}

pub fn get_location(parsed_source: &ParsedSource, pos: SourcePos) -> Location {
  get_text_info_location(
    parsed_source.specifier(),
    parsed_source.text_info(),
    pos,
  )
}

pub fn get_text_info_location(
  specifier: &str,
  text_info: &SourceTextInfo,
  pos: SourcePos,
) -> Location {
  // todo(#150): for some reason we're using a display indent width of 4
  let line_and_column_index =
    text_info.line_and_column_display_with_indent_width(pos, 4);
  Location {
    filename: specifier.to_string(),
    // todo(#150): make 0-indexed
    line: line_and_column_index.line_number,
    col: line_and_column_index.column_number - 1,
  }
}

pub fn module_export_name_value(
  module_export_name: &ModuleExportName,
) -> String {
  match module_export_name {
    ModuleExportName::Ident(ident) => ident.sym.to_string(),
    ModuleExportName::Str(str) => str.value.to_string(),
  }
}

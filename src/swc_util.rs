// Copyright 2020-2021 the Deno authors. All rights reserved. MIT license.

use deno_ast::swc::common::comments::CommentKind;
use deno_ast::swc::common::comments::Comments;
use deno_ast::swc::common::BytePos;
use deno_ast::swc::common::Span;
use deno_ast::ParsedSource;
use regex::Regex;

use crate::js_doc::JsDoc;
use crate::node::Location;

lazy_static! {
  static ref JS_DOC_RE: Regex = Regex::new(r#"\s*\* ?"#).unwrap();
}

pub fn js_doc_for_span(parsed_source: &ParsedSource, span: &Span) -> JsDoc {
  let comments = parsed_source
    .comments()
    .get_leading(span.lo())
    .unwrap_or_else(Vec::new);
  if let Some(js_doc_comment) = comments.iter().rev().find(|comment| {
    comment.kind == CommentKind::Block && comment.text.starts_with('*')
  }) {
    let txt = js_doc_comment
      .text
      .split('\n')
      .map(|line| JS_DOC_RE.replace(line, "").to_string())
      .collect::<Vec<String>>()
      .join("\n");

    let txt = txt.trim_start().trim_end().to_string();

    txt.into()
  } else {
    JsDoc::default()
  }
}

pub fn get_location(parsed_source: &ParsedSource, pos: BytePos) -> Location {
  // todo(#150): for some reason we're using a display indent width of 4
  let line_and_column_index = parsed_source
    .source()
    .line_and_column_display_with_indent_width(pos, 4);
  Location {
    filename: parsed_source.specifier().to_string(),
    // todo(#150): make 0-indexed
    line: line_and_column_index.line_number,
    col: line_and_column_index.column_number - 1,
  }
}

// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

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
  static ref JS_DOC_RE: Regex = Regex::new(r"^\s*\* ?").unwrap();
}

pub(crate) fn is_false(b: &bool) -> bool {
  !b
}

fn parse_js_doc(js_doc_comment: &Comment) -> JsDoc {
  remove_stars_from_js_doc(&js_doc_comment.text).into()
}

fn remove_stars_from_js_doc(text: &str) -> String {
  text
    .split('\n')
    .map(|line| JS_DOC_RE.replace(line, "").to_string())
    .collect::<Vec<String>>()
    .join("\n")
    .trim()
    .to_string()
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
  let shebang_length = parsed_source
    .module()
    .shebang
    .as_ref()
    .map_or(0, |shebang| shebang.len());
  let pos_leading_comment =
    parsed_source.comments().leading_map().keys().min()?;

  let comments = if shebang_length > 0 {
    parsed_source
      .comments()
      .get_leading(SourcePos::unsafely_from_byte_pos(*pos_leading_comment))
  } else {
    parsed_source.get_leading_comments()
  };
  if let Some(js_doc_comment) = comments.and_then(|comments| {
    comments.iter().find(|comment| {
      comment.kind == CommentKind::Block && comment.text.starts_with('*')
    })
  }) {
    let js_doc = parse_js_doc(js_doc_comment);
    if js_doc
      .tags
      .iter()
      .any(|tag| matches!(tag, JsDocTag::Module { .. }))
    {
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
    parsed_source.specifier().as_str(),
    parsed_source.text_info_lazy(),
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
  let byte_index = pos.as_byte_index(text_info.range().start);
  Location {
    filename: specifier.into(),
    // todo(#150): make 0-indexed
    line: line_and_column_index.line_number,
    col: line_and_column_index.column_number - 1,
    byte_index,
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

/// If the jsdoc has an `@internal` or `@ignore` tag.
pub fn has_ignorable_js_doc_tag(js_doc: &JsDoc) -> bool {
  js_doc
    .tags
    .iter()
    .any(|t| *t == JsDocTag::Ignore || *t == JsDocTag::Internal)
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn remove_stars_from_js_doc_works() {
    assert_eq!(
      remove_stars_from_js_doc(
        "/**
 * This module provides the `Result` class
 */"
      ),
      "/**
This module provides the `Result` class
/"
    );
    assert_eq!(
      remove_stars_from_js_doc(
        r#"/**
 * # Program
 *
 * description
 *
 * ## Usage
 *
 * @example
 * ```ts
 * import * as mod from "program";
 */"#
      ),
      r#"/**
# Program

description

## Usage

@example
```ts
import * as mod from "program";
/"#
    );
    assert_eq!(
      remove_stars_from_js_doc(
        r#"/**
 * # Program
 * **Example:**
 *
 * example1
 */
"#
      ),
      r#"/**
# Program
**Example:**

example1
/"#
    )
  }
}

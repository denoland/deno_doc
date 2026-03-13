// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use deno_ast::SourceTextInfo;
use deno_ast::oxc::ast::ast::Comment;
use deno_ast::oxc::ast::ast::ModuleExportName;
use deno_ast::oxc::span::Span;
use deno_graph::symbols::EsModuleInfo;
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

fn parse_js_doc(
  source_text: &str,
  comment: &Comment,
  module_info: &EsModuleInfo,
) -> JsDoc {
  let content_span = comment.content_span();
  let text =
    &source_text[content_span.start as usize..content_span.end as usize];
  JsDoc::new(remove_stars_from_js_doc(text), module_info)
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
  module_info: &EsModuleInfo,
  span: Span,
) -> JsDoc {
  let source_text = module_info.source_text();
  let comments = module_info.comments();
  if let Some(js_doc_comment) = comments
    .iter()
    .rev()
    .filter(|c| c.attached_to == span.start && c.is_leading() && c.is_block())
    .find(|c| {
      let content_span = c.content_span();
      let text =
        &source_text[content_span.start as usize..content_span.end as usize];
      text.starts_with('*')
    })
  {
    parse_js_doc(source_text, js_doc_comment, module_info)
  } else {
    JsDoc::default()
  }
}

pub(crate) fn js_doc_for_range(
  module_info: &EsModuleInfo,
  span: Span,
) -> Option<JsDoc> {
  let js_doc = js_doc_for_range_include_ignore(module_info, span);
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
  module_info: &EsModuleInfo,
) -> Option<Option<(JsDoc, Span)>> {
  let source_text = module_info.source_text();
  let comments = module_info.comments();
  let statements = module_info.statements();

  // Find the start of the first statement (or end of file)
  let first_stmt_start = statements
    .first()
    .map(|s| {
      use deno_ast::oxc::span::GetSpan;
      s.span().start
    })
    .unwrap_or(u32::MAX);

  // Find leading block comments before the first statement that look like JSDoc
  let js_doc_comment = comments.iter().find(|comment| {
    comment.span.start < first_stmt_start && comment.is_block() && {
      let content_span = comment.content_span();
      let text =
        &source_text[content_span.start as usize..content_span.end as usize];
      text.starts_with('*')
    }
  })?;

  let js_doc = parse_js_doc(source_text, js_doc_comment, module_info);
  if js_doc
    .tags
    .iter()
    .any(|tag| matches!(tag, JsDocTag::Module { .. }))
  {
    if js_doc.tags.contains(&JsDocTag::Ignore) {
      return Some(None);
    }
    return Some(Some((js_doc, js_doc_comment.span)));
  }
  None
}

pub fn get_location(module_info: &EsModuleInfo, pos: u32) -> Location {
  get_text_info_location(
    module_info.specifier().as_str(),
    module_info.source_text_info(),
    pos,
  )
}

pub fn get_text_info_location(
  specifier: &str,
  text_info: &SourceTextInfo,
  pos: u32,
) -> Location {
  let byte_index = pos as usize;
  let line_and_column_index = text_info.line_and_column_display(byte_index);

  // Adjust column for tab indentation (tabs count as 4 spaces for display,
  // matching the old SWC behavior with indent_width=4)
  let line_start_byte = byte_index - (line_and_column_index.column_number - 1);
  let text = text_info.text();
  let col = if line_start_byte < text.len() {
    let line_prefix = &text[line_start_byte..byte_index];
    let mut col = 0usize;
    for ch in line_prefix.chars() {
      if ch == '\t' {
        // Round up to next multiple of 4
        col = (col + 4) & !3;
      } else {
        col += 1;
      }
    }
    col
  } else {
    line_and_column_index.column_number - 1
  };

  Location {
    filename: specifier.into(),
    // todo(#150): make 0-indexed
    line: line_and_column_index.line_number,
    col,
    byte_index,
  }
}

pub fn module_export_name_value(
  module_export_name: &ModuleExportName,
) -> String {
  match module_export_name {
    ModuleExportName::IdentifierName(ident) => ident.name.to_string(),
    ModuleExportName::IdentifierReference(ident) => ident.name.to_string(),
    ModuleExportName::StringLiteral(str) => str.value.to_string(),
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

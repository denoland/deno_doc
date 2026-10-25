// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::collections::HashSet;

use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;
use deno_ast::SourceTextInfo;
use deno_ast::swc::common::comments::CommentKind;
use deno_graph::symbols::EsModuleInfo;

/// Ignores the diagnostics on the next line of code.
const IGNORE_DIRECTIVE: &str = "deno-doc-ignore";
/// Ignores the diagnostics in the entire file.
const IGNORE_FILE_DIRECTIVE: &str = "deno-doc-ignore-file";

/// The diagnostic codes a directive applies to. An empty list means the
/// directive applies to every diagnostic.
type Codes = Vec<String>;

fn codes_match(codes: &Codes, code: &str) -> bool {
  codes.is_empty() || codes.iter().any(|c| c == code)
}

/// Parses the codes that follow a directive, ex. the
/// `missing-jsdoc, missing-explicit-type` of
/// `// deno-doc-ignore missing-jsdoc, missing-explicit-type -- some reason`.
///
/// Returns `None` when the text does not start with the directive.
fn parse_directive_codes<'a>(
  comment_text: &'a str,
  directive: &str,
) -> Option<Codes> {
  let text = comment_text.trim();
  let rest = text.strip_prefix(directive)?;
  // ensure `deno-doc-ignoreable` doesn't match `deno-doc-ignore`
  if !rest.is_empty() && !rest.starts_with([' ', '\t', ',']) {
    return None;
  }
  // everything after a `--` is a human readable reason
  let rest: &'a str = match rest.split_once("--") {
    Some((codes, _reason)) => codes,
    None => rest,
  };
  Some(
    rest
      .split([' ', '\t', ','])
      .filter(|code| !code.is_empty())
      .map(|code| code.to_string())
      .collect(),
  )
}

/// The `// deno-doc-ignore` and `// deno-doc-ignore-file` directives found in
/// a module.
#[derive(Debug, Default, Clone)]
pub struct IgnoreDirectives {
  /// Codes of the file level directive, if one is present.
  file: Option<Codes>,
  /// Codes of the line directives by their 0-indexed line.
  lines: HashMap<usize, Codes>,
  /// 0-indexed lines that contain nothing but a comment.
  comment_lines: HashSet<usize>,
}

impl IgnoreDirectives {
  /// Whether a diagnostic with the provided code on the provided 0-indexed
  /// line is ignored.
  ///
  /// A line directive applies to the next line of code, so any comment lines
  /// between the directive and the code are skipped. This allows placing the
  /// directive above or below a JSDoc comment.
  pub fn is_ignored(&self, line_index: usize, code: &str) -> bool {
    if self
      .file
      .as_ref()
      .is_some_and(|codes| codes_match(codes, code))
    {
      return true;
    }

    let mut line_index = line_index;
    while line_index > 0 {
      line_index -= 1;
      if let Some(codes) = self.lines.get(&line_index)
        && codes_match(codes, code)
      {
        return true;
      }
      if !self.comment_lines.contains(&line_index) {
        break; // hit a line of code
      }
    }

    false
  }
}

pub fn parse_ignore_directives(module_info: &EsModuleInfo) -> IgnoreDirectives {
  let text_info = module_info.source().text_info_lazy();
  let comments = module_info.source().comments().get_vec();

  let mut directives = IgnoreDirectives::default();
  for comment in &comments {
    for line_index in comment_only_lines(text_info, comment.range()) {
      directives.comment_lines.insert(line_index);
    }
  }

  for comment in &comments {
    if comment.kind != CommentKind::Line {
      continue;
    }
    let line_index = text_info.line_index(comment.start());
    if !directives.comment_lines.contains(&line_index) {
      continue; // trailing comment on a line of code
    }

    if let Some(codes) =
      parse_directive_codes(&comment.text, IGNORE_FILE_DIRECTIVE)
    {
      // only recognize a file directive at the top of the file
      if (0..line_index).all(|line_index| {
        directives.comment_lines.contains(&line_index)
          || text_info.line_text(line_index).trim().is_empty()
      }) {
        directives.file = Some(codes);
      }
    } else if let Some(codes) =
      parse_directive_codes(&comment.text, IGNORE_DIRECTIVE)
    {
      directives.lines.insert(line_index, codes);
    }
  }

  directives
}

/// The 0-indexed lines of a comment that contain no code.
fn comment_only_lines(
  text_info: &SourceTextInfo,
  range: SourceRange,
) -> std::ops::RangeInclusive<usize> {
  let start_line = text_info.line_index(range.start);
  let end_line = text_info.line_index(range.end);
  let has_code_before = !text_info
    .range_text(&SourceRange::new(
      text_info.line_start(start_line),
      range.start,
    ))
    .trim()
    .is_empty();
  let has_code_after = !text_info
    .range_text(&SourceRange::new(range.end, text_info.line_end(end_line)))
    .trim()
    .is_empty();

  let start_line = if has_code_before {
    start_line + 1
  } else {
    start_line
  };
  let end_line = if has_code_after {
    end_line.saturating_sub(1)
  } else {
    end_line
  };
  start_line..=end_line
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_codes() {
    assert_eq!(
      parse_directive_codes(" deno-doc-ignore", "deno-doc-ignore"),
      Some(vec![])
    );
    assert_eq!(
      parse_directive_codes(
        " deno-doc-ignore missing-jsdoc",
        "deno-doc-ignore"
      ),
      Some(vec!["missing-jsdoc".to_string()])
    );
    assert_eq!(
      parse_directive_codes(
        " deno-doc-ignore missing-jsdoc, missing-explicit-type -- because",
        "deno-doc-ignore"
      ),
      Some(vec![
        "missing-jsdoc".to_string(),
        "missing-explicit-type".to_string()
      ])
    );
    assert_eq!(
      parse_directive_codes(" deno-doc-ignoreable", "deno-doc-ignore"),
      None
    );
    assert_eq!(parse_directive_codes(" other", "deno-doc-ignore"), None);
  }
}

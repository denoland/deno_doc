// Copyright 2020-2023 the Deno authors. All rights reserved. MIT license.

//! Support for "hidden" lines in example code blocks, mirroring the behavior
//! of rustdoc's documentation tests.
//!
//! Within a JavaScript/TypeScript code block, a line that begins with `# `
//! (hash followed by a space) — or that consists solely of `#` — is *hidden*:
//! it is removed from the rendered documentation but is still part of the
//! runnable example. This lets authors keep boilerplate (imports, setup, etc.)
//! runnable without cluttering the rendered output. A line beginning with `##`
//! is an escape for a leading literal `#` and is rendered with a single `#`.
//!
//! See <https://doc.rust-lang.org/rustdoc/write-documentation/documentation-tests.html#hiding-portions-of-the-example>.

/// Whether a fenced code block's language denotes a runnable JavaScript or
/// TypeScript example.
///
/// Hidden-line processing is intentionally limited to these languages so that
/// other code blocks (for instance shell snippets that legitimately begin a
/// line with `#`) are rendered verbatim.
fn is_example_lang(lang: &str) -> bool {
  matches!(
    lang,
    "js"
      | "javascript"
      | "mjs"
      | "cjs"
      | "jsx"
      | "ts"
      | "typescript"
      | "mts"
      | "cts"
      | "tsx"
  )
}

/// The result of processing the body of an example code block for hidden lines.
pub struct ExampleCode {
  /// The code shown to the reader, with hidden lines removed and `##` escapes
  /// resolved to a single `#`.
  pub displayed: String,
  /// The full, runnable code: hidden-line markers are stripped but every line
  /// is retained, so a copied snippet still executes as the author intended.
  pub copyable: String,
}

/// Processes the body of a fenced code block, splitting it into the code that
/// should be displayed and the full runnable code.
///
/// Returns `None` when `lang` is not a runnable JavaScript/TypeScript language
/// (see [`is_example_lang`]), in which case the block should be rendered as-is.
pub fn process_example_code(
  lang: Option<&str>,
  code: &str,
) -> Option<ExampleCode> {
  // The info string may carry additional attributes (e.g. `ts ignore`); only
  // the first token denotes the language.
  let lang = lang.unwrap_or("").split_whitespace().next().unwrap_or("");
  if !is_example_lang(lang) {
    return None;
  }

  let mut displayed = String::new();
  let mut copyable = String::new();

  for line in code.split_inclusive('\n') {
    let (content, newline) = match line.strip_suffix('\n') {
      Some(content) => (content, "\n"),
      None => (line, ""),
    };
    let trimmed = content.trim_start();
    let indent = &content[..content.len() - trimmed.len()];

    if let Some(rest) = trimmed.strip_prefix("##") {
      // Escaped literal `#`: render and copy with a single leading `#`.
      displayed.push_str(indent);
      displayed.push('#');
      displayed.push_str(rest);
      displayed.push_str(newline);

      copyable.push_str(indent);
      copyable.push('#');
      copyable.push_str(rest);
      copyable.push_str(newline);
    } else if trimmed == "#" {
      // Hidden empty line: kept in the runnable code only.
      copyable.push_str(indent);
      copyable.push_str(newline);
    } else if let Some(rest) = trimmed.strip_prefix("# ") {
      // Hidden line: kept in the runnable code only.
      copyable.push_str(indent);
      copyable.push_str(rest);
      copyable.push_str(newline);
    } else {
      displayed.push_str(line);
      copyable.push_str(line);
    }
  }

  Some(ExampleCode {
    displayed,
    copyable,
  })
}

#[cfg(test)]
mod tests {
  use super::*;

  fn displayed(lang: Option<&str>, code: &str) -> Option<String> {
    process_example_code(lang, code).map(|c| c.displayed)
  }

  fn copyable(lang: Option<&str>, code: &str) -> Option<String> {
    process_example_code(lang, code).map(|c| c.copyable)
  }

  #[test]
  fn hides_lines() {
    let code = "# import { add } from \"./mod.ts\";\nadd(1, 2);\n";
    assert_eq!(displayed(Some("ts"), code).unwrap(), "add(1, 2);\n");
    assert_eq!(
      copyable(Some("ts"), code).unwrap(),
      "import { add } from \"./mod.ts\";\nadd(1, 2);\n"
    );
  }

  #[test]
  fn hides_bare_hash_line() {
    let code = "const x = 1;\n#\nconst y = 2;\n";
    assert_eq!(
      displayed(Some("js"), code).unwrap(),
      "const x = 1;\nconst y = 2;\n"
    );
    assert_eq!(
      copyable(Some("js"), code).unwrap(),
      "const x = 1;\n\nconst y = 2;\n"
    );
  }

  #[test]
  fn escapes_double_hash() {
    let code = "## not hidden\n";
    assert_eq!(displayed(Some("ts"), code).unwrap(), "# not hidden\n");
    assert_eq!(copyable(Some("ts"), code).unwrap(), "# not hidden\n");
  }

  #[test]
  fn preserves_indentation() {
    let code = "function f() {\n  # const secret = 1;\n  return 2;\n}\n";
    assert_eq!(
      displayed(Some("ts"), code).unwrap(),
      "function f() {\n  return 2;\n}\n"
    );
    assert_eq!(
      copyable(Some("ts"), code).unwrap(),
      "function f() {\n  const secret = 1;\n  return 2;\n}\n"
    );
  }

  #[test]
  fn leaves_private_fields_and_attributes_alone() {
    // `#field` (no following space) is a private field, not a hidden line.
    let code = "class C {\n  #x = 1;\n}\n";
    assert_eq!(displayed(Some("ts"), code).unwrap(), code);
    assert_eq!(copyable(Some("ts"), code).unwrap(), code);
  }

  #[test]
  fn preserves_shebang() {
    let code = "#!/usr/bin/env -S deno run\nconsole.log(1);\n";
    assert_eq!(displayed(Some("ts"), code).unwrap(), code);
  }

  #[test]
  fn ignores_non_example_languages() {
    // Shell snippets routinely begin a line with `#`; leave them untouched.
    let code = "# install\ndeno install\n";
    assert!(process_example_code(Some("sh"), code).is_none());
    assert!(process_example_code(Some("bash"), code).is_none());
    assert!(process_example_code(None, code).is_none());
  }

  #[test]
  fn handles_attributes_in_info_string() {
    let code = "# hidden;\nshown;\n";
    assert_eq!(displayed(Some("ts ignore"), code).unwrap(), "shown;\n");
  }

  #[test]
  fn handles_missing_trailing_newline() {
    let code = "# hidden;\nshown;";
    assert_eq!(displayed(Some("ts"), code).unwrap(), "shown;");
    assert_eq!(copyable(Some("ts"), code).unwrap(), "hidden;\nshown;");
  }
}

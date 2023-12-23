// Copied and modified from https://github.com/kivikakk/comrak/blob/main/src/plugins/syntect.rs

//! Adapter for the Syntect syntax highlighter plugin.

use comrak::adapters::SyntaxHighlighterAdapter;
use comrak::html;
use std::collections::HashMap;
use std::io::{self, Write};
use syntect::easy::HighlightLines;
use syntect::highlighting::ThemeSet;
use syntect::html::{
  append_highlighted_html_for_styled_line, IncludeBackground,
};
use syntect::parsing::{SyntaxReference, SyntaxSet};
use syntect::util::LinesWithEndings;
use syntect::Error;

#[derive(Debug)]
/// Syntect syntax highlighter plugin.
pub struct SyntectAdapter {
  pub syntax_set: SyntaxSet,
  pub theme_set: ThemeSet,
}

const THEME: &str = "InspiredGitHub";

impl SyntectAdapter {
  fn highlight_html(
    &self,
    code: &str,
    syntax: &SyntaxReference,
  ) -> Result<String, Error> {
    // syntect::html::highlighted_html_for_string, without the opening/closing <pre>.
    let theme = &self.theme_set.themes[THEME];
    let mut highlighter = HighlightLines::new(syntax, theme);

    let mut output = String::new();
    for line in LinesWithEndings::from(code) {
      let regions = highlighter.highlight_line(line, &self.syntax_set)?;
      append_highlighted_html_for_styled_line(
        &regions[..],
        IncludeBackground::No,
        &mut output,
      )?;
    }
    Ok(output)
  }
}

impl SyntaxHighlighterAdapter for SyntectAdapter {
  fn write_highlighted(
    &self,
    output: &mut dyn Write,
    lang: Option<&str>,
    code: &str,
  ) -> io::Result<()> {
    let lang = match lang {
      Some(l) if !l.is_empty() => l,
      _ => "Plain Text",
    };

    let syntax =
      self
        .syntax_set
        .find_syntax_by_token(lang)
        .unwrap_or_else(|| {
          self
            .syntax_set
            .find_syntax_by_first_line(code)
            .unwrap_or_else(|| self.syntax_set.find_syntax_plain_text())
        });

    match self.highlight_html(code, syntax) {
      Ok(highlighted_code) => output.write_all(highlighted_code.as_bytes()),
      Err(_) => output.write_all(code.as_bytes()),
    }
  }

  fn write_pre_tag(
    &self,
    output: &mut dyn Write,
    attributes: HashMap<String, String>,
  ) -> io::Result<()> {
    html::write_opening_tag(output, "pre", attributes)
  }

  fn write_code_tag(
    &self,
    output: &mut dyn Write,
    attributes: HashMap<String, String>,
  ) -> io::Result<()> {
    html::write_opening_tag(output, "code", attributes)
  }
}

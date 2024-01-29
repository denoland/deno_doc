// Copied and modified from https://github.com/kivikakk/comrak/blob/main/src/plugins/syntect.rs

//! Adapter for the Syntect syntax highlighter plugin.

use comrak::adapters::HeadingAdapter;
use comrak::adapters::HeadingMeta;
use comrak::adapters::SyntaxHighlighterAdapter;
use comrak::nodes::Sourcepos;
use std::collections::HashMap;
use std::io::Write;
use std::sync::Arc;
use std::sync::Mutex;
use syntect::easy::HighlightLines;
use syntect::highlighting::ThemeSet;
use syntect::html::append_highlighted_html_for_styled_line;
use syntect::html::IncludeBackground;
use syntect::parsing::SyntaxReference;
use syntect::parsing::SyntaxSet;
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
  ) -> std::io::Result<()> {
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
  ) -> std::io::Result<()> {
    comrak::html::write_opening_tag(output, "pre", attributes)
  }

  fn write_code_tag(
    &self,
    output: &mut dyn Write,
    attributes: HashMap<String, String>,
  ) -> std::io::Result<()> {
    comrak::html::write_opening_tag(output, "code", attributes)
  }
}

#[derive(Default)]
pub struct HeadingToCAdapter {
  toc: Arc<Mutex<Vec<(u8, String, String)>>>,
  anchorizer: Arc<Mutex<comrak::html::Anchorizer>>,
}

impl HeadingToCAdapter {
  pub fn get_toc(&self) -> Vec<(u8, String, String)> {
    let lock = self.toc.lock().unwrap();
    lock.clone()
  }
}

impl HeadingAdapter for HeadingToCAdapter {
  fn enter(
    &self,
    output: &mut dyn Write,
    heading: &HeadingMeta,
    _sourcepos: Option<Sourcepos>,
  ) -> std::io::Result<()> {
    let mut anchorizer = self.anchorizer.lock().unwrap();

    let anchor = anchorizer.anchorize(heading.content.clone());
    writeln!(output, r#"<h{} id="{anchor}">"#, heading.level)?;

    let mut lock = self.toc.lock().unwrap();
    lock.push((heading.level, heading.content.clone(), anchor));

    Ok(())
  }

  fn exit(
    &self,
    output: &mut dyn Write,
    heading: &HeadingMeta,
  ) -> std::io::Result<()> {
    writeln!(output, "</h{}>", heading.level)?;
    Ok(())
  }
}

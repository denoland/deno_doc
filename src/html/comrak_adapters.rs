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
  pub show_line_numbers: bool,
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

    let mut line_numbers = String::new();
    let mut lines = String::new();

    for (i, line) in LinesWithEndings::from(code).enumerate() {
      let n = i + 1;

      if self.show_line_numbers {
        line_numbers.push_str(&format!(
          r##"<a href="#L{n}" class="no_color block">{n}</a>"##,
        ));

        lines.push_str(&format!(
          r#"<span id="L{n}" class="block target:bg-yellow-200">"#
        ));
      }

      let regions = highlighter.highlight_line(line, &self.syntax_set)?;
      append_highlighted_html_for_styled_line(
        &regions[..],
        IncludeBackground::No,
        &mut lines,
      )?;

      if self.show_line_numbers {
        lines.push_str("</span>");
      }
    }

    if self.show_line_numbers {
      Ok(format!(
        r##"<div class="border-r-2 border-stone-300 pr-1 text-right flex-none">{line_numbers}</div><div class="grow">{lines}</div>"##
      ))
    } else {
      Ok(lines)
    }
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
    mut attributes: HashMap<String, String>,
  ) -> std::io::Result<()> {
    attributes
      .entry("class".into())
      .or_default()
      .push_str(" highlight");
    comrak::html::write_opening_tag(output, "pre", attributes)
  }

  fn write_code_tag(
    &self,
    output: &mut dyn Write,
    mut attributes: HashMap<String, String>,
  ) -> std::io::Result<()> {
    if self.show_line_numbers {
      attributes
        .entry("class".into())
        .or_default()
        .push_str(" flex gap-2");
    }
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

#[cfg(feature = "ammonia")]
pub type URLRewriter = Arc<
  dyn (Fn(Option<&deno_ast::ModuleSpecifier>, &str) -> String) + Send + Sync,
>;

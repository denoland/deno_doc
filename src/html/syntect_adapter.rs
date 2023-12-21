// Copied and modified from https://github.com/kivikakk/comrak/blob/main/src/plugins/syntect.rs

//! Adapter for the Syntect syntax highlighter plugin.

use comrak::adapters::SyntaxHighlighterAdapter;
use comrak::html;
use std::collections::{hash_map, HashMap};
use std::io::{self, Write};
use syntect::easy::HighlightLines;
use syntect::highlighting::{Color, ThemeSet};
use syntect::html::{
  append_highlighted_html_for_styled_line, ClassStyle, ClassedHTMLGenerator,
  IncludeBackground,
};
use syntect::parsing::{SyntaxReference, SyntaxSet};
use syntect::util::LinesWithEndings;
use syntect::Error;

#[derive(Debug)]
/// Syntect syntax highlighter plugin.
pub(crate) struct SyntectAdapter {
  pub theme: Option<String>,
  pub syntax_set: SyntaxSet,
  pub theme_set: ThemeSet,
}

impl SyntectAdapter {
  fn highlight_html(
    &self,
    code: &str,
    syntax: &SyntaxReference,
  ) -> Result<String, Error> {
    match &self.theme {
      Some(theme) => {
        // syntect::html::highlighted_html_for_string, without the opening/closing <pre>.
        let theme = &self.theme_set.themes[theme];
        let mut highlighter = HighlightLines::new(syntax, theme);

        let bg = theme.settings.background.unwrap_or(Color::WHITE);

        let mut output = String::new();
        for line in LinesWithEndings::from(code) {
          let regions = highlighter.highlight_line(line, &self.syntax_set)?;
          append_highlighted_html_for_styled_line(
            &regions[..],
            IncludeBackground::IfDifferent(bg),
            &mut output,
          )?;
        }
        Ok(output)
      }
      None => {
        // fall back to HTML classes.
        let mut html_generator = ClassedHTMLGenerator::new_with_class_style(
          syntax,
          &self.syntax_set,
          ClassStyle::Spaced,
        );
        for line in LinesWithEndings::from(code) {
          html_generator.parse_html_for_line_which_includes_newline(line)?;
        }
        Ok(html_generator.finalize())
      }
    }
  }
}

impl SyntaxHighlighterAdapter for SyntectAdapter {
  fn write_highlighted(
    &self,
    output: &mut dyn Write,
    lang: Option<&str>,
    code: &str,
  ) -> io::Result<()> {
    let fallback_syntax = "Plain Text";

    let lang: &str = match lang {
      Some(l) if !l.is_empty() => l,
      _ => fallback_syntax,
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
    match &self.theme {
      Some(theme) => {
        let theme = &self.theme_set.themes[theme];
        let colour = theme.settings.background.unwrap_or(Color::WHITE);

        let style = format!(
          "background-color:#{:02x}{:02x}{:02x};",
          colour.r, colour.g, colour.b
        );

        let mut pre_attributes = SyntectPreAttributes::new(attributes, &style);
        html::write_opening_tag(output, "pre", pre_attributes.iter_mut())
      }
      None => {
        let mut attributes: HashMap<&str, &str> = HashMap::new();
        attributes.insert("class", "syntax-highlighting");
        html::write_opening_tag(output, "pre", attributes)
      }
    }
  }

  fn write_code_tag(
    &self,
    output: &mut dyn Write,
    attributes: HashMap<String, String>,
  ) -> io::Result<()> {
    html::write_opening_tag(output, "code", attributes)
  }
}

struct SyntectPreAttributes {
  syntect_style: String,
  attributes: HashMap<String, String>,
}

impl SyntectPreAttributes {
  fn new(attributes: HashMap<String, String>, syntect_style: &str) -> Self {
    Self {
      syntect_style: syntect_style.into(),
      attributes,
    }
  }

  fn iter_mut(&mut self) -> SyntectPreAttributesIter {
    SyntectPreAttributesIter {
      iter_mut: self.attributes.iter_mut(),
      syntect_style: &self.syntect_style,
      style_written: false,
    }
  }
}

struct SyntectPreAttributesIter<'a> {
  iter_mut: hash_map::IterMut<'a, String, String>,
  syntect_style: &'a str,
  style_written: bool,
}

impl<'a> Iterator for SyntectPreAttributesIter<'a> {
  type Item = (&'a str, &'a str);

  fn next(&mut self) -> Option<Self::Item> {
    match self.iter_mut.next() {
      Some((k, v)) if k == "style" && !self.style_written => {
        self.style_written = true;
        v.insert_str(0, self.syntect_style);
        Some((k, v))
      }
      Some((k, v)) => Some((k, v)),
      None if !self.style_written => {
        self.style_written = true;
        Some(("style", self.syntect_style))
      }
      None => None,
    }
  }
}

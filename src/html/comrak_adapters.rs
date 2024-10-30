// Copied and modified from https://github.com/kivikakk/comrak/blob/main/src/plugins/syntect.rs

//! Adapter for the Syntect syntax highlighter plugin.

#![allow(clippy::print_stderr)]

#[cfg(any(
  not(any(feature = "syntect", feature = "tree-sitter")),
  all(feature = "syntect", feature = "tree-sitter")
))]
compile_error!(
  "Either feature \"syntect\" or \"tree-sitter\" must be enabled, not both or neither."
);

use comrak::adapters::HeadingAdapter;
use comrak::adapters::HeadingMeta;
use comrak::adapters::SyntaxHighlighterAdapter;
use comrak::nodes::Sourcepos;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::io::Write;
use std::sync::Arc;
use std::sync::Mutex;

#[derive(Debug)]
/// Syntect syntax highlighter plugin.
pub struct HighlightAdapter {
  #[cfg(feature = "syntect")]
  pub syntax_set: syntect::parsing::SyntaxSet,
  #[cfg(feature = "syntect")]
  pub theme_set: syntect::highlighting::ThemeSet,
  #[cfg(feature = "tree-sitter")]
  pub language_cb:
    fn(&str) -> Option<&'static tree_sitter_highlight::HighlightConfiguration>,
  pub show_line_numbers: bool,
}

impl HighlightAdapter {
  fn highlight_html<'a, I, H>(
    &self,
    iter: I,
    mut highlighter: H,
  ) -> Result<String, anyhow::Error>
  where
    I: Iterator<Item = &'a str>,
    H: FnMut(&mut String, &str) -> Result<(), anyhow::Error>,
  {
    let mut line_numbers = String::new();
    let mut lines = String::new();

    for (i, line) in iter.enumerate() {
      let n = i + 1;

      if self.show_line_numbers {
        line_numbers.push_str(&format!(
          r##"<a href="#L{n}" class="no_color block">{n}</a>"##,
        ));

        lines.push_str(&format!(
          r#"<span id="L{n}" class="block target:bg-yellow-200">"#
        ));
      }

      highlighter(&mut lines, line)?;

      if self.show_line_numbers {
        lines.push_str("</span>");
      }
    }

    if self.show_line_numbers {
      Ok(format!(
        r##"<div class="lineNumbers">{line_numbers}</div><div class="grow overflow-x-auto">{lines}</div>"##
      ))
    } else {
      Ok(lines)
    }
  }

  fn write_button(
    &self,
    output: &mut dyn Write,
    source: &str,
  ) -> std::io::Result<()> {
    write!(output, "</code>")?;
    write!(
      output,
      r#"<button class="context_button" data-copy="{}">{}{}</button>"#,
      html_escape::encode_double_quoted_attribute(source),
      include_str!("./templates/icons/copy.svg"),
      include_str!("./templates/icons/check.svg")
    )
  }
}

impl SyntaxHighlighterAdapter for HighlightAdapter {
  #[cfg(all(feature = "syntect", not(feature = "tree-sitter")))]
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

    let theme = &self.theme_set.themes["InspiredGitHub"];
    let mut highlighter = syntect::easy::HighlightLines::new(syntax, theme);

    match self.highlight_html(
      syntect::util::LinesWithEndings::from(code),
      |lines, line| {
        let regions = highlighter.highlight_line(line, &self.syntax_set)?;

        syntect::html::append_highlighted_html_for_styled_line(
          &regions,
          syntect::html::IncludeBackground::No,
          lines,
        )?;

        Ok(())
      },
    ) {
      Ok(highlighted_code) => output.write_all(highlighted_code.as_bytes())?,
      Err(_) => output.write_all(code.as_bytes())?,
    }

    self.write_button(output, code)
  }

  #[cfg(all(feature = "tree-sitter", not(feature = "syntect")))]
  fn write_highlighted(
    &self,
    output: &mut dyn Write,
    lang: Option<&str>,
    code: &str,
  ) -> std::io::Result<()> {
    let lang = lang.unwrap_or_default();
    let config = (self.language_cb)(lang);
    let source = code.as_bytes();
    if let Some(config) = config {
      let mut highlighter = tree_sitter_highlight::Highlighter::new();
      let res = highlighter.highlight(config, source, None, self.language_cb);
      match res {
        Ok(highlighter) => {
          let mut renderer = tree_sitter_highlight::HtmlRenderer::new();
          match renderer.render(highlighter, source, &|highlight| {
            crate::html::tree_sitter::classes(highlight)
          }) {
            Ok(()) => {
              let html = self
                .highlight_html(renderer.lines(), |lines, line| {
                  lines.push_str(line);
                  Ok(())
                })
                .unwrap();

              output.write_all(html.as_bytes())?;
              return self.write_button(output, code);
            }
            Err(err) => {
              eprintln!("Error rendering code: {}", err);
            }
          };
        }
        Err(err) => {
          eprintln!("Error highlighting code: {}", err);
        }
      }
    }
    comrak::html::escape(output, source)?;
    self.write_button(output, code)
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
        .push_str(" !flex gap-2");
    }
    comrak::html::write_opening_tag(output, "code", attributes)
  }
}

#[derive(Debug)]
pub struct ToCEntry {
  pub level: u8,
  pub content: String,
  pub anchor: String,
}

#[derive(Default)]
pub struct Anchorizer {
  map: HashMap<String, i32>,
  itoa_buffer: itoa::Buffer,
}

impl Anchorizer {
  /// Returns a String that has been converted into an anchor using the GFM algorithm.
  /// This replaces comrak's implementation to improve the performance.
  /// @see https://docs.rs/comrak/latest/comrak/struct.Anchorizer.html#method.anchorize
  pub fn anchorize(&mut self, s: &str) -> String {
    let mut s = REJECTED_CHARS
      .replace_all(&s.to_lowercase(), "")
      .replace(' ', "-");

    if let Some(count) = self.map.get_mut(&s) {
      let a = self.itoa_buffer.format(*count);
      s.push('-');
      s.push_str(a);

      *count += 1;
    } else {
      self.map.insert(s.clone(), 1);
    }

    s
  }
}

#[derive(Clone)]
pub struct HeadingToCAdapter {
  toc: Arc<Mutex<Vec<ToCEntry>>>,
  offset: Arc<Mutex<u8>>,
  anchorizer: Arc<Mutex<Anchorizer>>,
}

impl Default for HeadingToCAdapter {
  fn default() -> Self {
    Self {
      toc: Arc::new(Mutex::new(vec![])),
      anchorizer: Arc::new(Mutex::new(Default::default())),
      offset: Arc::new(Mutex::new(0)),
    }
  }
}

lazy_static! {
  static ref REJECTED_CHARS: regex::Regex =
    regex::Regex::new(r"[^\p{L}\p{M}\p{N}\p{Pc} -]").unwrap();
}

impl HeadingToCAdapter {
  pub fn anchorize(&self, content: &str) -> String {
    let mut anchorizer = self.anchorizer.lock().unwrap();
    anchorizer.anchorize(content)
  }

  pub fn add_entry(&self, level: u8, content: &str, anchor: &str) {
    let mut toc = self.toc.lock().unwrap();
    let mut offset = self.offset.lock().unwrap();

    *offset = level;

    if toc.last().map_or(true, |toc| toc.content != content) {
      toc.push(ToCEntry {
        level,
        content: content.to_owned(),
        anchor: anchor.to_owned(),
      });
    }
  }

  pub fn render(self) -> Option<String> {
    let toc = Arc::into_inner(self.toc).unwrap().into_inner().unwrap();

    if toc.is_empty() {
      return None;
    }

    let mut toc_content = vec!["<ul>".to_string()];
    let mut current_level = toc.iter().map(|entry| entry.level).min().unwrap();

    let mut level_diff = 0;
    for entry in toc {
      match current_level.cmp(&entry.level) {
        Ordering::Equal => {}
        Ordering::Less => {
          level_diff += 1;
          toc_content.push(r#"<li><ul>"#.to_string());
          current_level = entry.level;
        }
        Ordering::Greater => {
          level_diff -= 1;
          toc_content.push("</ul></li>".to_string());
          current_level = entry.level;
        }
      }

      toc_content.push(format!(
        r##"<li><a href="#{}" title="{}">{}</a></li>"##,
        entry.anchor,
        html_escape::encode_double_quoted_attribute(&entry.content),
        entry.content
      ));
    }

    for _ in 0..level_diff {
      toc_content.push("</ul></li>".to_string());
    }

    toc_content.push(String::from("</ul>"));

    Some(toc_content.join(""))
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
    let offset = self.offset.lock().unwrap();

    let anchor = anchorizer.anchorize(&heading.content);
    writeln!(output, r#"<h{} id="{anchor}">"#, heading.level)?;

    let mut toc = self.toc.lock().unwrap();
    toc.push(ToCEntry {
      level: heading.level + *offset,
      content: heading.content.clone(),
      anchor,
    });

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
pub type URLRewriter =
  Arc<dyn (Fn(Option<&crate::html::ShortPath>, &str) -> String) + Send + Sync>;

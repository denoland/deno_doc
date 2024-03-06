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
        r##"<div class="border-r-2 border-stone-300 pr-1 text-right flex-none">{line_numbers}</div><div class="grow overflow-x-auto">{lines}</div>"##
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
      r#"<button class="button" data-copy="{}">{}</button>"#,
      html_escape::encode_safe(source),
      include_str!("./templates/icons/copy.svg")
    )?;
    write!(output, "<code>")
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
          &regions[..],
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

  #[cfg(all(not(feature = "syntect"), not(feature = "tree-sitter")))]
  fn write_highlighted(
    &self,
    output: &mut dyn Write,
    _lang: Option<&str>,
    code: &str,
  ) -> std::io::Result<()> {
    let html = self
      .highlight_html(code.lines(), |lines, line| {
        lines.push_str(&format!("{line}\n"));

        Ok(())
      })
      .unwrap();

    output.write_all(html.as_bytes())?;
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
        .push_str(" flex gap-2");
    }
    comrak::html::write_opening_tag(output, "code", attributes)
  }
}

#[derive(Default)]
pub struct HeadingToCAdapter {
  toc: Mutex<Vec<(u8, String, String)>>,
  anchorizer: Mutex<comrak::html::Anchorizer>,
}

impl HeadingToCAdapter {
  pub fn into_toc(self) -> Vec<(u8, String, String)> {
    self.toc.into_inner().unwrap()
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

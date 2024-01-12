use std::sync::OnceLock;

use comrak::adapters::SyntaxHighlighterAdapter;
use tree_sitter_highlight::Highlight;
use tree_sitter_highlight::HighlightConfiguration;

macro_rules! highlighter {
    [$($name:literal -> $class:literal,)*] => {
      /// The capture names to configure on the highlighter. If this is not
      /// configured correctly, the highlighter will not work.
      pub const CAPTURE_NAMES: &[&str] = &[$($name),*];
      const CLASSES: &[&str] = &[$(concat!("class=\"", $class, "\"")),*];
    };
}

highlighter! [
  "attribute" -> "pl-c1",
  "comment" -> "pl-c",
  "constant.builtin" -> "pl-c1",
  "constant" -> "pl-c1",
  "constructor" -> "pl-v",
  "embedded" -> "pl-s1",
  "function" -> "pl-en",
  "keyword" -> "pl-k",
  "operator" -> "pl-c1",
  "property" -> "pl-c1",
  "string" -> "pl-s",
  "tag" -> "pl-ent",
  "type" -> "pl-smi",
  "variable.builtin" -> "pl-smi",
];

fn classes(highlight: Highlight) -> &'static [u8] {
  CLASSES[highlight.0].as_bytes()
}

pub struct TreeSitterHighlighter {
  language_cb: fn(&str) -> Option<&'static HighlightConfiguration>,
}

impl TreeSitterHighlighter {
  pub fn new(
    language_cb: fn(&str) -> Option<&'static HighlightConfiguration>,
  ) -> Self {
    Self { language_cb }
  }
}

impl SyntaxHighlighterAdapter for TreeSitterHighlighter {
  fn write_highlighted<'a>(
    &self,
    output: &mut dyn std::io::prelude::Write,
    lang: Option<&str>,
    code: &'a str,
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
          match renderer
            .render(highlighter, source, &|highlight| classes(highlight))
          {
            Ok(()) => return output.write_all(&renderer.html),
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
    comrak::html::escape(output, source)
  }

  fn write_pre_tag(
    &self,
    output: &mut dyn std::io::prelude::Write,
    mut attributes: std::collections::HashMap<String, String>,
  ) -> std::io::Result<()> {
    attributes
      .entry("class".into())
      .or_default()
      .push_str(" highlight");
    comrak::html::write_opening_tag(output, "pre", attributes)
  }

  fn write_code_tag(
    &self,
    output: &mut dyn std::io::prelude::Write,
    attributes: std::collections::HashMap<String, String>,
  ) -> std::io::Result<()> {
    comrak::html::write_opening_tag(output, "code", attributes)
  }
}

pub fn tree_sitter_language_cb(
  lang: &str,
) -> Option<&'static HighlightConfiguration> {
  for lang in lang.split(',') {
    let cfg = match lang.trim() {
      #[cfg(feature = "tree-sitter-javascript")]
      "js" | "javascript" => tree_sitter_language_javascript(),
      #[cfg(feature = "tree-sitter-javascript")]
      "jsx" => tree_sitter_language_jsx(),
      #[cfg(feature = "tree-sitter-typescript")]
      "ts" | "typescript" => tree_sitter_language_typescript(),
      #[cfg(feature = "tree-sitter-typescript")]
      "tsx" => tree_sitter_language_tsx(),
      #[cfg(feature = "tree-sitter-json")]
      "json" => tree_sitter_language_json(),
      #[cfg(feature = "tree-sitter-css")]
      "css" => tree_sitter_language_css(),
      #[cfg(feature = "tree-sitter-md")]
      "md" | "markdown" => tree_sitter_language_markdown(),
      #[cfg(feature = "tree-sitter-toml")]
      "toml" => tree_sitter_language_toml(),
      #[cfg(feature = "tree-sitter-regex")]
      "regex" => tree_sitter_language_regex(),
      _ => continue,
    };
    return Some(cfg);
  }
  None
}

#[cfg(feature = "tree-sitter-javascript")]
pub fn tree_sitter_language_javascript() -> &'static HighlightConfiguration {
  static CONFIG: OnceLock<HighlightConfiguration> = OnceLock::new();
  CONFIG.get_or_init(|| {
    let mut config = HighlightConfiguration::new(
      tree_sitter_javascript::language(),
      tree_sitter_javascript::HIGHLIGHT_QUERY,
      tree_sitter_javascript::INJECTION_QUERY,
      tree_sitter_javascript::LOCALS_QUERY,
    )
    .expect("failed to initialize tree_sitter_javascript highlighter");
    config.configure(CAPTURE_NAMES);
    config
  })
}

#[cfg(feature = "tree-sitter-javascript")]
pub fn tree_sitter_language_jsx() -> &'static HighlightConfiguration {
  static CONFIG: OnceLock<HighlightConfiguration> = OnceLock::new();
  CONFIG.get_or_init(|| {
    let mut config = HighlightConfiguration::new(
      tree_sitter_javascript::language(),
      format!(
        "{} {}",
        tree_sitter_javascript::HIGHLIGHT_QUERY,
        tree_sitter_javascript::JSX_HIGHLIGHT_QUERY
      )
      .leak(),
      tree_sitter_javascript::INJECTION_QUERY,
      tree_sitter_javascript::LOCALS_QUERY,
    )
    .expect("failed to initialize tree_sitter_javascript highlighter");
    config.configure(CAPTURE_NAMES);
    config
  })
}

#[cfg(feature = "tree-sitter-typescript")]
pub fn tree_sitter_language_typescript() -> &'static HighlightConfiguration {
  static CONFIG: OnceLock<HighlightConfiguration> = OnceLock::new();
  CONFIG.get_or_init(|| {
    let mut config = HighlightConfiguration::new(
      tree_sitter_typescript::language_typescript(),
      format!(
        "{} {}",
        tree_sitter_javascript::HIGHLIGHT_QUERY,
        tree_sitter_typescript::HIGHLIGHT_QUERY
      )
      .leak(),
      tree_sitter_javascript::INJECTION_QUERY,
      format!(
        "{} {}",
        tree_sitter_javascript::LOCALS_QUERY,
        tree_sitter_typescript::LOCALS_QUERY
      )
      .leak(),
    )
    .expect("failed to initialize tree_sitter_typescript highlighter");
    config.configure(CAPTURE_NAMES);
    config
  })
}

#[cfg(feature = "tree-sitter-typescript")]
pub fn tree_sitter_language_tsx() -> &'static HighlightConfiguration {
  static CONFIG: OnceLock<HighlightConfiguration> = OnceLock::new();
  CONFIG.get_or_init(|| {
    let mut config = HighlightConfiguration::new(
      tree_sitter_typescript::language_tsx(),
      format!(
        "{} {} {}",
        tree_sitter_javascript::HIGHLIGHT_QUERY,
        tree_sitter_javascript::JSX_HIGHLIGHT_QUERY,
        tree_sitter_typescript::HIGHLIGHT_QUERY,
      )
      .leak(),
      tree_sitter_javascript::INJECTION_QUERY,
      format!(
        "{} {}",
        tree_sitter_javascript::LOCALS_QUERY,
        tree_sitter_typescript::LOCALS_QUERY
      )
      .leak(),
    )
    .expect("failed to initialize tree_sitter_typescript highlighter");
    config.configure(CAPTURE_NAMES);
    config
  })
}

#[cfg(feature = "tree-sitter-json")]
fn tree_sitter_language_json() -> &'static HighlightConfiguration {
  static CONFIG: OnceLock<HighlightConfiguration> = OnceLock::new();
  CONFIG.get_or_init(|| {
    let mut config = HighlightConfiguration::new(
      tree_sitter_json::language(),
      tree_sitter_json::HIGHLIGHT_QUERY,
      "",
      "",
    )
    .expect("failed to initialize tree_sitter_json highlighter");
    config.configure(CAPTURE_NAMES);
    config
  })
}

#[cfg(feature = "tree-sitter-css")]
fn tree_sitter_language_css() -> &'static HighlightConfiguration {
  static CONFIG: OnceLock<HighlightConfiguration> = OnceLock::new();
  CONFIG.get_or_init(|| {
    let mut config = HighlightConfiguration::new(
      tree_sitter_css::language(),
      tree_sitter_css::HIGHLIGHTS_QUERY,
      "",
      "",
    )
    .expect("failed to initialize tree_sitter_css highlighter");
    config.configure(CAPTURE_NAMES);
    config
  })
}

#[cfg(feature = "tree-sitter-md")]
fn tree_sitter_language_markdown() -> &'static HighlightConfiguration {
  static CONFIG: OnceLock<HighlightConfiguration> = OnceLock::new();
  CONFIG.get_or_init(|| {
    let mut config = HighlightConfiguration::new(
      tree_sitter_md::language(),
      tree_sitter_md::HIGHLIGHT_QUERY_BLOCK,
      tree_sitter_md::INJECTION_QUERY_BLOCK,
      "",
    )
    .expect("failed to initialize tree_sitter_md highlighter");
    config.configure(CAPTURE_NAMES);
    config
  })
}

#[cfg(feature = "tree-sitter-toml")]
fn tree_sitter_language_toml() -> &'static HighlightConfiguration {
  static CONFIG: OnceLock<HighlightConfiguration> = OnceLock::new();
  CONFIG.get_or_init(|| {
    let mut config = HighlightConfiguration::new(
      tree_sitter_toml::language(),
      tree_sitter_toml::HIGHLIGHT_QUERY,
      "",
      "",
    )
    .expect("failed to initialize tree_sitter_toml highlighter");
    config.configure(CAPTURE_NAMES);
    config
  })
}

#[cfg(feature = "tree-sitter-regex")]
fn tree_sitter_language_regex() -> &'static HighlightConfiguration {
  static CONFIG: OnceLock<HighlightConfiguration> = OnceLock::new();
  CONFIG.get_or_init(|| {
    let mut config = HighlightConfiguration::new(
      tree_sitter_regex::language(),
      tree_sitter_regex::HIGHLIGHTS_QUERY,
      "",
      "",
    )
    .expect("failed to initialize tree_sitter_regex highlighter");
    config.configure(CAPTURE_NAMES);
    config
  })
}

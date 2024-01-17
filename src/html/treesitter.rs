use std::io::Write;
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
  "number" -> "pl-c1",
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
  show_line_numbers: bool,
}

impl TreeSitterHighlighter {
  pub fn new(
    language_cb: fn(&str) -> Option<&'static HighlightConfiguration>,
    show_line_numbers: bool,
  ) -> Self {
    Self {
      language_cb,
      show_line_numbers,
    }
  }
}

impl SyntaxHighlighterAdapter for TreeSitterHighlighter {
  fn write_highlighted(
    &self,
    output: &mut dyn std::io::prelude::Write,
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
          match renderer
            .render(highlighter, source, &|highlight| classes(highlight))
          {
            Ok(()) => {
              if self.show_line_numbers {
                let mut line_numbers = String::from(
                  r#"<div class="border-r-2 border-stone-300 pr-1 text-right flex-none">"#,
                );

                let mut lines = String::from(r#"<div class="grow">"#);

                for (i, line) in renderer.lines().enumerate() {
                  let n = i + 1;
                  line_numbers.push_str(&format!(
                    r##"<a href="#L{n}" class="no_color block">{n}</a>"##,
                  ));

                  lines.push_str(&format!(
                    r#"<span id="L{n}" class="block target:bg-yellow-200">{line}</span>"#
                  ));
                }

                line_numbers.push_str("</div>");
                output.write_all(line_numbers.as_bytes())?;

                lines.push_str("</div>");
                return output.write_all(lines.as_bytes());
              } else {
                return output.write_all(&renderer.html);
              }
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
    mut attributes: std::collections::HashMap<String, String>,
  ) -> std::io::Result<()> {
    if self.show_line_numbers {
      attributes
        .entry("class".into())
        .or_default()
        .push_str(" highlight flex gap-2");
    }
    comrak::html::write_opening_tag(output, "code", attributes)
  }
}

pub fn tree_sitter_language_cb(
  lang: &str,
) -> Option<&'static HighlightConfiguration> {
  for lang in lang.split(',') {
    let cfg = match lang.trim() {
      "js" | "javascript" => tree_sitter_language_javascript(),
      "jsx" => tree_sitter_language_jsx(),
      "ts" | "typescript" => tree_sitter_language_typescript(),
      "tsx" => tree_sitter_language_tsx(),
      "json" | "jsonc" => tree_sitter_language_json(),
      "css" => tree_sitter_language_css(),
      "md" | "markdown" => tree_sitter_language_markdown(),
      "toml" => tree_sitter_language_toml(),
      "regex" => tree_sitter_language_regex(),
      "rs" | "rust" => tree_sitter_language_rust(),
      _ => continue,
    };
    return Some(cfg);
  }
  None
}

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

fn tree_sitter_language_rust() -> &'static HighlightConfiguration {
  static CONFIG: OnceLock<HighlightConfiguration> = OnceLock::new();
  CONFIG.get_or_init(|| {
    let mut config = HighlightConfiguration::new(
      tree_sitter_rust::language(),
      tree_sitter_rust::HIGHLIGHT_QUERY,
      tree_sitter_rust::INJECTIONS_QUERY,
      "",
    )
    .expect("failed to initialize tree_sitter_rust highlighter");
    config.configure(CAPTURE_NAMES);
    config
  })
}

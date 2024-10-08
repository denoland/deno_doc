use std::sync::OnceLock;

use tree_sitter_highlight::Highlight;
use tree_sitter_highlight::HighlightConfiguration;

macro_rules! highlighter {
    [$($name:literal -> $class:literal,)*] => {
      /// The capture names to configure on the highlighter. If this is not
      /// configured correctly, the highlighter will not work.
      pub const CAPTURE_NAMES: &[&str] = &[$($name),*];
      const CLASSES_ATTRIBUTES: &[&str] = &[$(concat!("class=\"", $class, "\"")),*];
      pub const CLASSES: &[&str] = &[$($class),*];
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

pub(crate) fn classes(highlight: Highlight) -> &'static [u8] {
  CLASSES_ATTRIBUTES[highlight.0].as_bytes()
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
      "xml" => tree_sitter_language_xml(),
      "dtd" => tree_sitter_language_dtd(),
      "regex" => tree_sitter_language_regex(),
      "rs" | "rust" => tree_sitter_language_rust(),
      "html" => tree_sitter_language_html(),
      "sh" | "bash" => tree_sitter_language_bash(),
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
      "javascript",
      tree_sitter_javascript::HIGHLIGHT_QUERY,
      tree_sitter_javascript::INJECTIONS_QUERY,
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
      "jsx",
      format!(
        "{} {}",
        tree_sitter_javascript::HIGHLIGHT_QUERY,
        tree_sitter_javascript::JSX_HIGHLIGHT_QUERY
      )
      .leak(),
      tree_sitter_javascript::INJECTIONS_QUERY,
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
      "typescript",
      format!(
        "{} {}",
        tree_sitter_javascript::HIGHLIGHT_QUERY,
        tree_sitter_typescript::HIGHLIGHTS_QUERY
      )
      .leak(),
      tree_sitter_javascript::INJECTIONS_QUERY,
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
      "tsx",
      format!(
        "{} {} {}",
        tree_sitter_javascript::HIGHLIGHT_QUERY,
        tree_sitter_javascript::JSX_HIGHLIGHT_QUERY,
        tree_sitter_typescript::HIGHLIGHTS_QUERY,
      )
      .leak(),
      tree_sitter_javascript::INJECTIONS_QUERY,
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
      "json",
      tree_sitter_json::HIGHLIGHTS_QUERY,
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
      "css",
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
      "markdown",
      tree_sitter_md::HIGHLIGHT_QUERY_BLOCK,
      tree_sitter_md::INJECTION_QUERY_BLOCK,
      "",
    )
    .expect("failed to initialize tree_sitter_md highlighter");
    config.configure(CAPTURE_NAMES);
    config
  })
}

fn tree_sitter_language_xml() -> &'static HighlightConfiguration {
  static CONFIG: OnceLock<HighlightConfiguration> = OnceLock::new();
  CONFIG.get_or_init(|| {
    let mut config = HighlightConfiguration::new(
      tree_sitter_xml::language_xml(),
      "xml",
      tree_sitter_xml::XML_HIGHLIGHT_QUERY,
      "",
      "",
    )
    .expect("failed to initialize tree_sitter_xml highlighter");
    config.configure(CAPTURE_NAMES);
    config
  })
}

fn tree_sitter_language_dtd() -> &'static HighlightConfiguration {
  static CONFIG: OnceLock<HighlightConfiguration> = OnceLock::new();
  CONFIG.get_or_init(|| {
    let mut config = HighlightConfiguration::new(
      tree_sitter_xml::language_dtd(),
      "dtd",
      tree_sitter_xml::DTD_HIGHLIGHT_QUERY,
      "",
      "",
    )
    .expect("failed to initialize tree_sitter_dtd highlighter");
    config.configure(CAPTURE_NAMES);
    config
  })
}

fn tree_sitter_language_regex() -> &'static HighlightConfiguration {
  static CONFIG: OnceLock<HighlightConfiguration> = OnceLock::new();
  CONFIG.get_or_init(|| {
    let mut config = HighlightConfiguration::new(
      tree_sitter_regex::language(),
      "regex",
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
      "rust",
      tree_sitter_rust::HIGHLIGHTS_QUERY,
      tree_sitter_rust::INJECTIONS_QUERY,
      "",
    )
    .expect("failed to initialize tree_sitter_rust highlighter");
    config.configure(CAPTURE_NAMES);
    config
  })
}

fn tree_sitter_language_html() -> &'static HighlightConfiguration {
  static CONFIG: OnceLock<HighlightConfiguration> = OnceLock::new();
  CONFIG.get_or_init(|| {
    let mut config = HighlightConfiguration::new(
      tree_sitter_html::language(),
      "html",
      tree_sitter_html::HIGHLIGHTS_QUERY,
      tree_sitter_html::INJECTIONS_QUERY,
      "",
    )
    .expect("failed to initialize tree_sitter_html highlighter");
    config.configure(CAPTURE_NAMES);
    config
  })
}

fn tree_sitter_language_bash() -> &'static HighlightConfiguration {
  static CONFIG: OnceLock<HighlightConfiguration> = OnceLock::new();
  CONFIG.get_or_init(|| {
    let mut config = HighlightConfiguration::new(
      tree_sitter_bash::language(),
      "bash",
      tree_sitter_bash::HIGHLIGHT_QUERY,
      "",
      "",
    )
    .expect("failed to initialize tree_sitter_bash highlighter");
    config.configure(CAPTURE_NAMES);
    config
  })
}

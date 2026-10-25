use crate::html::ShortPath;
use comrak::Arena;
use comrak::adapters::SyntaxHighlighterAdapter;
use comrak::nodes::AstNode;
use comrak::nodes::NodeValue;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Write;
use std::sync::Arc;

pub const COMRAK_STYLESHEET: &str = include_str!("./templates/comrak.gen.css");
pub const COMRAK_STYLESHEET_FILENAME: &str = "comrak.css";

pub type NodeHook = Box<
  dyn for<'a> Fn(
      &'a Arena<'a>,
      &'a AstNode<'a>,
      &comrak::Options,
      &comrak::options::Plugins,
    ) + Send
    + Sync,
>;

fn walk_node<'a>(
  node_hook: &NodeHook,
  arena: &'a Arena<'a>,
  node: &'a AstNode<'a>,
  options: &comrak::Options,
  plugins: &comrak::options::Plugins,
) {
  for child in node.children() {
    node_hook(arena, child, options, plugins);
    walk_node(node_hook, arena, child, options, plugins);
  }
}

fn walk_node_title<'a>(node: &'a AstNode<'a>) {
  for child in node.children() {
    if matches!(
      child.data.borrow().value,
      NodeValue::Document
        | NodeValue::Paragraph
        | NodeValue::Heading(_)
        | NodeValue::Text(_)
        | NodeValue::Code(_)
        | NodeValue::HtmlInline(_)
        | NodeValue::Emph
        | NodeValue::Strong
        | NodeValue::Strikethrough
        | NodeValue::Superscript
        | NodeValue::Link(_)
        | NodeValue::Math(_)
        | NodeValue::Escaped
        | NodeValue::WikiLink(_)
        | NodeValue::Underline
        | NodeValue::SoftBreak
    ) {
      walk_node_title(child);
    } else {
      // delete the node
      child.detach();
    }
  }
}

pub fn render_node<'a>(
  node: &'a AstNode<'a>,
  options: &comrak::Options,
  plugins: &comrak::options::Plugins,
) -> String {
  let mut html = String::new();
  comrak::format_html_with_plugins(node, options, &mut html, plugins).unwrap();
  html
}

/// Returns comrak options with standard extensions enabled.
/// Used for parsing and rendering across the codebase.
pub fn default_options() -> comrak::Options<'static> {
  let mut options = comrak::Options::default();
  options.extension.autolink = true;
  options.extension.description_lists = true;
  options.extension.footnotes = true;
  options.extension.strikethrough = true;
  options.extension.superscript = true;
  options.extension.table = true;
  options.extension.tasklist = true;
  options
}

pub fn strip(md: &str) -> String {
  let mut options = default_options();
  options.render.escape = true;

  let arena = Arena::new();
  let root = comrak::parse_document(&arena, md, &options);

  fn collect_text<'a>(node: &'a AstNode<'a>, output: &mut String) {
    match node.data.borrow().value {
      NodeValue::Text(ref literal) => output.push_str(literal),
      NodeValue::Code(comrak::nodes::NodeCode { ref literal, .. }) => {
        output.push_str(literal);
      }
      NodeValue::LineBreak | NodeValue::SoftBreak => output.push(' '),
      _ => {
        for n in node.children() {
          collect_text(n, output);
        }
      }
    }
  }

  let mut text = String::new();
  collect_text(root, &mut text);
  text
}

pub type HtmlClean = Box<dyn Fn(String) -> String + Send + Sync>;

pub fn create_renderer(
  syntax_highlighter: Option<Arc<dyn SyntaxHighlighterAdapter>>,
  node_hook: Option<NodeHook>,
  clean: Option<HtmlClean>,
) -> super::jsdoc::MarkdownRenderer {
  let has_clean = clean.is_some();

  let renderer = move |md: &str,
                       title_only: bool,
                       _file_path: Option<ShortPath>,
                       anchorizer: super::jsdoc::Anchorizer|
        -> Option<String> {
    let mut options = default_options();
    options.render.escape = !has_clean;
    options.render.r#unsafe = has_clean; // its fine because we run the cleaner afterwards

    let mut plugins = comrak::options::Plugins::default();
    let heading_adapter = ComrakHeadingAdapter(anchorizer);
    let highlight_adapter =
      ComrakHighlightWrapperAdapter(syntax_highlighter.clone());

    if !title_only {
      plugins.render.codefence_syntax_highlighter = Some(&highlight_adapter);
      plugins.render.heading_adapter = Some(&heading_adapter);
    }

    let html = {
      let arena = Arena::new();
      let root = comrak::parse_document(&arena, md, &options);

      if title_only {
        walk_node_title(root);

        {
          let child = root.first_child()?;
          render_node(child, &options, &plugins)
        }
      } else {
        if let Some(node_hook) = &node_hook {
          walk_node(node_hook, &arena, root, &options, &plugins);
        }

        render_node(root, &options, &plugins)
      }
    };

    let html = if let Some(clean) = &clean {
      clean(html)
    } else {
      html
    };

    Some(html)
  };

  Arc::new(renderer)
}

pub struct ComrakHeadingAdapter(super::jsdoc::Anchorizer);

impl comrak::adapters::HeadingAdapter for ComrakHeadingAdapter {
  fn enter(
    &self,
    output: &mut dyn Write,
    heading: &comrak::adapters::HeadingMeta,
    _sourcepos: Option<comrak::nodes::Sourcepos>,
  ) -> std::fmt::Result {
    let anchor = self.0(heading.content.clone(), heading.level);
    // The anchorizer's character class keeps `"`, `<` and `&` (its ` -_` is a
    // range over U+0020..=U+005F, not three literals), so a heading anchor has
    // to be escaped before it goes into an attribute. Symbol ids arrive
    // pre-escaped from `sanitize_id_part`; anchors built from markdown
    // headings do not.
    let anchor = html_escape::encode_quoted_attribute(&anchor);

    write!(
      output,
      r##"<h{level} id="{anchor}" class="anchorable"><a href="#{anchor}" class="anchor" aria-label="Anchor" tabindex="-1">{link}</a>"##,
      level = heading.level,
      link = include_str!("./templates/icons/link.svg"),
    )
  }

  fn exit(
    &self,
    output: &mut dyn Write,
    heading: &comrak::adapters::HeadingMeta,
  ) -> std::fmt::Result {
    writeln!(output, "</h{}>", heading.level)
  }
}

pub struct ComrakHighlightWrapperAdapter(
  pub Option<Arc<dyn SyntaxHighlighterAdapter>>,
);

impl SyntaxHighlighterAdapter for ComrakHighlightWrapperAdapter {
  fn write_highlighted(
    &self,
    output: &mut dyn Write,
    lang: Option<&str>,
    code: &str,
  ) -> std::fmt::Result {
    // The markdown parser hands us the info string up to the first whitespace,
    // so a comma-separated directive such as `ts, no-eval` arrives as `ts,`.
    // Normalize it to the bare language token so both hidden-line processing and
    // the downstream highlighter recognize it.
    let lang = lang.map(crate::util::example_code::language_token);

    // Resolve any "hidden" lines (rustdoc style): they are removed from the
    // displayed code but kept in the copyable form so copied snippets still
    // run. Non-example languages are left untouched.
    let processed = crate::util::example_code::process_example_code(lang, code);
    let (displayed, copyable) = match &processed {
      Some(example) => (example.displayed.as_str(), example.copyable.as_str()),
      None => (code, code),
    };

    if let Some(adapter) = &self.0 {
      adapter.write_highlighted(output, lang, displayed)?;
    } else {
      comrak::html::escape(output, displayed)?;
    }

    write!(output, "</code>")?;
    write!(
      output,
      r#"<button class="copyButton" data-copy="{}">{}{}</button>"#,
      html_escape::encode_double_quoted_attribute(copyable),
      include_str!("./templates/icons/copy.svg"),
      include_str!("./templates/icons/check.svg"),
    )?;
    write!(output, "<code>")
  }

  fn write_pre_tag(
    &self,
    output: &mut dyn Write,
    mut attributes: HashMap<&'static str, Cow<'_, str>>,
  ) -> std::fmt::Result {
    attributes.insert("class", Cow::Borrowed("highlight"));
    if let Some(adapter) = &self.0 {
      adapter.write_pre_tag(output, attributes)
    } else {
      comrak::html::write_opening_tag(output, "pre", attributes)
    }
  }

  fn write_code_tag(
    &self,
    output: &mut dyn Write,
    attributes: HashMap<&'static str, Cow<'_, str>>,
  ) -> std::fmt::Result {
    if let Some(adapter) = &self.0 {
      adapter.write_code_tag(output, attributes)
    } else {
      comrak::html::write_opening_tag(output, "code", attributes)
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn render(md: &str) -> String {
    let renderer = create_renderer(None, None, None);
    let anchorizer: super::super::jsdoc::Anchorizer =
      Arc::new(|content: String, _level: u8| content);
    renderer(md, false, None, anchorizer).unwrap()
  }

  #[test]
  fn hides_example_code_lines() {
    let html = render("```ts\n# const x = 1;\nconsole.log(x);\n```");

    // The displayed code (before the closing `</code>`) omits the hidden line.
    let displayed = html.split("</code>").next().unwrap();
    assert!(!displayed.contains("const x = 1;"));
    assert!(displayed.contains("console.log(x);"));

    // The copy button retains the full, runnable snippet.
    assert!(html.contains("data-copy="));
    assert!(html.contains("const x = 1;"));
  }

  #[test]
  fn does_not_hide_non_example_code_lines() {
    let html = render("```sh\n# install\ndeno install\n```");
    let displayed = html.split("</code>").next().unwrap();
    assert!(displayed.contains("# install"));
  }

  #[test]
  fn escapes_double_hash_in_example_code() {
    let html = render("```ts\n## shown\n```");
    let displayed = html.split("</code>").next().unwrap();
    assert!(displayed.contains("# shown"));
    assert!(!displayed.contains("## shown"));
  }

  #[test]
  fn renders_footnotes() {
    let html = render("Some claim.[^1]\n\n[^1]: The source of the claim.");

    assert!(html.contains(r#"class="footnotes""#), "html: {html}");
    assert!(html.contains(r#"class="footnote-ref""#), "html: {html}");
    assert!(html.contains(r#"class="footnote-backref""#), "html: {html}");
    assert!(html.contains("The source of the claim."), "html: {html}");
  }

  #[test]
  fn strips_footnotes() {
    let stripped = strip("Some claim.[^1]\n\n[^1]: The source of the claim.");

    // The reference marker disappears and the definition text is kept as
    // plain text; no HTML leaks through and stripping must not panic.
    assert!(stripped.contains("Some claim."), "stripped: {stripped}");
    assert!(
      stripped.contains("The source of the claim."),
      "stripped: {stripped}"
    );
    assert!(!stripped.contains('<'), "stripped: {stripped}");
    assert!(!stripped.contains("[^1]"), "stripped: {stripped}");
  }

  #[test]
  fn comma_in_info_string_is_treated_as_example() {
    // A comma-separated directive (`ts, no-eval`) leaves the parser handing us
    // `ts,`; it must still be recognized as a `ts` example so hidden lines are
    // processed (regression test for broken highlighting on commas).
    let html = render("```ts, no-eval\n# const x = 1;\nconsole.log(x);\n```");
    let displayed = html.split("</code>").next().unwrap();
    assert!(!displayed.contains("const x = 1;"));
    assert!(displayed.contains("console.log(x);"));
    // The full snippet is still copyable.
    assert!(html.contains("const x = 1;"));
  }
}

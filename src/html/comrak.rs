use crate::html::ShortPath;
use comrak::Arena;
use comrak::adapters::SyntaxHighlighterAdapter;
use comrak::nodes::AstNode;
use comrak::nodes::NodeValue;
use std::collections::HashMap;
use std::io::BufWriter;
use std::io::Write;
use std::sync::Arc;

pub const COMRAK_STYLESHEET: &str = include_str!("./templates/comrak.gen.css");
pub const COMRAK_STYLESHEET_FILENAME: &str = "comrak.css";

pub type NodeHook = Box<
  dyn for<'a> Fn(
      &'a Arena<AstNode<'a>>,
      &'a AstNode<'a>,
      &comrak::Options,
      &comrak::Plugins,
    ) + Send
    + Sync,
>;

fn walk_node<'a>(
  node_hook: &NodeHook,
  arena: &'a Arena<AstNode<'a>>,
  node: &'a AstNode<'a>,
  options: &comrak::Options,
  plugins: &comrak::Plugins,
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

/// Append the inline plain-text contribution of `node` to `out`.
/// Counts `NodeValue::Text` and `NodeValue::Code` literals, and treats
/// `NodeValue::SoftBreak` as a single space. Other inline containers
/// (Emph, Strong, Link, etc.) descend into their children.
fn collect_inline_plain_text<'a>(node: &'a AstNode<'a>, out: &mut String) {
  match &node.data.borrow().value {
    NodeValue::Text(t) => out.push_str(t),
    NodeValue::Code(c) => out.push_str(&c.literal),
    NodeValue::SoftBreak => out.push(' '),
    _ => {
      for child in node.children() {
        collect_inline_plain_text(child, out);
      }
    }
  }
}

/// Shorten the inline tree rooted at `node` so that, in document order,
/// the cumulative plain-text contribution is exactly `target` bytes.
/// Returns true once `*offset >= target`, signalling to callers that the
/// rest of the tree should be detached.
fn shorten_inline_to<'a>(
  node: &'a AstNode<'a>,
  offset: &mut usize,
  target: usize,
) -> bool {
  if *offset >= target {
    return true;
  }

  enum Kind {
    Text,
    Code,
    SoftBreak,
    Container,
  }

  let kind = match &node.data.borrow().value {
    NodeValue::Text(_) => Kind::Text,
    NodeValue::Code(_) => Kind::Code,
    NodeValue::SoftBreak => Kind::SoftBreak,
    _ => Kind::Container,
  };

  match kind {
    Kind::Text => {
      let mut data = node.data.borrow_mut();
      if let NodeValue::Text(t) = &mut data.value {
        let len = t.len();
        if *offset + len <= target {
          *offset += len;
        } else {
          let take = target - *offset;
          t.replace_range(take.., "");
          *offset = target;
        }
      }
    }
    Kind::Code => {
      let mut data = node.data.borrow_mut();
      if let NodeValue::Code(c) = &mut data.value {
        let len = c.literal.len();
        if *offset + len <= target {
          *offset += len;
        } else {
          let take = target - *offset;
          c.literal.replace_range(take.., "");
          *offset = target;
        }
      }
    }
    Kind::SoftBreak => {
      *offset += 1;
    }
    Kind::Container => {
      let children: Vec<_> = node.children().collect();
      let mut done = false;
      for child in children {
        if done {
          child.detach();
        } else {
          done = shorten_inline_to(child, offset, target);
        }
      }
    }
  }

  *offset >= target
}

/// If the first paragraph's trimmed plain text ends with `':'` and
/// contains a `'.'`, shorten the paragraph so the rendered output ends
/// right after the last `'.'`. Does nothing otherwise. See
/// <https://github.com/denoland/deno_doc/issues/633>.
fn shorten_title_summary_trailing_colon<'a>(paragraph: &'a AstNode<'a>) {
  let mut plain = String::new();
  collect_inline_plain_text(paragraph, &mut plain);

  let trimmed = plain.trim_end_matches(|c: char| c.is_ascii_whitespace());
  if !trimmed.ends_with(':') {
    return;
  }

  let Some(last_dot) = trimmed.rfind('.') else {
    return;
  };

  let target = last_dot + 1;
  let mut offset = 0;
  shorten_inline_to(paragraph, &mut offset, target);
}

pub fn render_node<'a>(
  node: &'a AstNode<'a>,
  options: &comrak::Options,
  plugins: &comrak::Plugins,
) -> String {
  let mut bw = BufWriter::new(Vec::new());
  comrak::format_html_with_plugins(node, options, &mut bw, plugins).unwrap();
  String::from_utf8(bw.into_inner().unwrap()).unwrap()
}

/// Returns comrak options with standard extensions enabled.
/// Used for parsing and rendering across the codebase.
pub fn default_options() -> comrak::Options<'static> {
  let mut options = comrak::Options::default();
  options.extension.autolink = true;
  options.extension.description_lists = true;
  options.extension.strikethrough = true;
  options.extension.superscript = true;
  options.extension.table = true;
  options.extension.tagfilter = true;
  options.extension.tasklist = true;
  options
}

pub fn strip(md: &str) -> String {
  let mut options = default_options();
  options.render.escape = true;

  let arena = Arena::new();
  let root = comrak::parse_document(&arena, md, &options);

  fn collect_text<'a>(node: &'a AstNode<'a>, output: &mut BufWriter<Vec<u8>>) {
    match node.data.borrow().value {
      NodeValue::Text(ref literal)
      | NodeValue::Code(comrak::nodes::NodeCode { ref literal, .. }) => {
        output.write_all(literal.as_bytes()).unwrap();
      }
      NodeValue::LineBreak | NodeValue::SoftBreak => {
        output.write_all(b" ").unwrap()
      }
      _ => {
        for n in node.children() {
          collect_text(n, output);
        }
      }
    }
  }

  let mut bw = BufWriter::new(Vec::new());
  collect_text(root, &mut bw);
  String::from_utf8(bw.into_inner().unwrap()).unwrap()
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
    options.render.unsafe_ = has_clean; // its fine because we run the cleaner afterwards

    let mut plugins = comrak::Plugins::default();
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

        if let Some(child) = root.first_child() {
          shorten_title_summary_trailing_colon(child);
          render_node(child, &options, &plugins)
        } else {
          return None;
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
  ) -> std::io::Result<()> {
    let anchor = self.0(heading.content.clone(), heading.level);

    writeln!(output, r#"<h{} id="{anchor}">"#, heading.level)
  }

  fn exit(
    &self,
    output: &mut dyn Write,
    heading: &comrak::adapters::HeadingMeta,
  ) -> std::io::Result<()> {
    writeln!(output, "</h{}>", heading.level)?;
    Ok(())
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
  ) -> std::io::Result<()> {
    if let Some(adapter) = &self.0 {
      adapter.write_highlighted(output, lang, code)?;
    } else {
      comrak::html::escape(output, code.as_bytes())?;
    }

    write!(output, "</code>")?;
    write!(
      output,
      r#"<button class="copyButton" data-copy="{}">{}{}</button>"#,
      html_escape::encode_double_quoted_attribute(code),
      include_str!("./templates/icons/copy.svg"),
      include_str!("./templates/icons/check.svg"),
    )?;
    write!(output, "<code>")
  }

  fn write_pre_tag(
    &self,
    output: &mut dyn Write,
    mut attributes: HashMap<String, String>,
  ) -> std::io::Result<()> {
    attributes.insert("class".to_string(), "highlight".to_string());
    if let Some(adapter) = &self.0 {
      adapter.write_pre_tag(output, attributes)
    } else {
      comrak::html::write_opening_tag(output, "pre", attributes)
    }
  }

  fn write_code_tag(
    &self,
    output: &mut dyn Write,
    attributes: HashMap<String, String>,
  ) -> std::io::Result<()> {
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

  fn render_title_only(md: &str) -> String {
    let mut options = default_options();
    options.render.escape = true;

    let arena = Arena::new();
    let root = comrak::parse_document(&arena, md, &options);
    walk_node_title(root);

    let Some(child) = root.first_child() else {
      return String::new();
    };
    shorten_title_summary_trailing_colon(child);
    render_node(child, &options, &comrak::Plugins::default())
  }

  #[test]
  fn title_summary_trailing_colon_with_period_shortens_to_last_period() {
    let html = render_title_only("This is a summary. It says:");
    assert_eq!(html.trim(), "<p>This is a summary.</p>");
  }

  #[test]
  fn title_summary_trailing_colon_without_period_left_unchanged() {
    let html = render_title_only("This is a summary:");
    assert_eq!(html.trim(), "<p>This is a summary:</p>");
  }

  #[test]
  fn title_summary_no_trailing_colon_left_unchanged() {
    let html = render_title_only("This is a summary.");
    assert_eq!(html.trim(), "<p>This is a summary.</p>");
  }

  #[test]
  fn title_summary_trailing_colon_after_code_span_shortens() {
    // Period before the code+colon tail — backtracking should drop the
    // code span and trailing colon entirely.
    let html = render_title_only("First. Then `something`:");
    assert_eq!(html.trim(), "<p>First.</p>");
  }

  #[test]
  fn title_summary_trailing_colon_after_emph_shortens() {
    let html = render_title_only("First. Then *emph*:");
    assert_eq!(html.trim(), "<p>First.</p>");
  }

  #[test]
  fn title_summary_period_inside_code_span_keeps_code_up_to_period() {
    // The last '.' lives inside a code span; shortening lands exactly at
    // the end of that span and drops the following text+colon.
    let html = render_title_only("Some `code.` text:");
    assert_eq!(html.trim(), "<p>Some <code>code.</code></p>");
  }
}

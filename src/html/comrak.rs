use crate::html::ShortPath;
use comrak::Arena;
use comrak::adapters::SyntaxHighlighterAdapter;
use comrak::nodes::AstNode;
use comrak::nodes::NodeValue;
use std::collections::HashMap;
use std::io::BufWriter;
use std::io::Write;
use std::rc::Rc;
use std::sync::Arc;

pub const COMRAK_STYLESHEET: &str = include_str!("./templates/comrak.gen.css");
pub const COMRAK_STYLESHEET_FILENAME: &str = "comrak.css";

pub type NodeHook = Box<
  dyn for<'a> Fn(
    &'a Arena<AstNode<'a>>,
    &'a AstNode<'a>,
    &comrak::Options,
    &comrak::Plugins,
  ),
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

pub fn render_node<'a>(
  node: &'a AstNode<'a>,
  options: &comrak::Options,
  plugins: &comrak::Plugins,
) -> String {
  let mut bw = BufWriter::new(Vec::new());
  comrak::format_html_with_plugins(node, options, &mut bw, plugins).unwrap();
  String::from_utf8(bw.into_inner().unwrap()).unwrap()
}

pub fn strip(md: &str) -> String {
  let mut options = comrak::Options::default();
  options.extension.autolink = true;
  options.extension.description_lists = true;
  options.extension.strikethrough = true;
  options.extension.superscript = true;
  options.extension.table = true;
  options.extension.tagfilter = true;
  options.extension.tasklist = true;
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

pub type HtmlClean = Box<dyn Fn(String) -> String>;

pub fn create_renderer(
  syntax_highlighter: Option<Arc<dyn SyntaxHighlighterAdapter>>,
  node_hook: Option<NodeHook>,
  clean: Option<HtmlClean>,
) -> super::jsdoc::MarkdownRenderer {
  let mut options = comrak::Options::default();
  options.extension.autolink = true;
  options.extension.description_lists = true;
  options.extension.strikethrough = true;
  options.extension.superscript = true;
  options.extension.table = true;
  options.extension.tagfilter = true;
  options.extension.tasklist = true;

  options.render.escape = clean.is_none();
  options.render.unsafe_ = clean.is_some(); // its fine because we run the cleaner afterwards

  let renderer = move |md: &str,
                       title_only: bool,
                       _file_path: Option<ShortPath>,
                       anchorizer: super::jsdoc::Anchorizer|
        -> Option<String> {
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

    let class_name = if title_only {
      "markdown_summary"
    } else {
      "markdown"
    };

    let html = format!(
      r#"<div class="{class_name}">{}</div>"#,
      if let Some(clean) = &clean {
        clean(html)
      } else {
        html
      }
    );

    Some(html)
  };

  Rc::new(renderer)
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

use crate::html::ShortPath;
use comrak::nodes::Ast;
use comrak::nodes::AstNode;
use comrak::nodes::NodeHtmlBlock;
use comrak::nodes::NodeValue;
use comrak::Arena;
use std::borrow::Cow;
use std::cell::RefCell;
use std::io::BufWriter;
use std::io::Write;
use std::rc::Rc;
use std::sync::Arc;

pub const COMRAK_STYLESHEET: &str = include_str!("./templates/comrak.gen.css");
pub const COMRAK_STYLESHEET_FILENAME: &str = "comrak.css";

pub type URLRewriter =
  Arc<dyn (Fn(Option<&ShortPath>, &str) -> String) + Send + Sync>;

thread_local! {
  static CURRENT_FILE: RefCell<Option<Option<ShortPath>>> = const { RefCell::new(None) };
  static URL_REWRITER: RefCell<Option<Option<URLRewriter>>> = const { RefCell::new(None) };
}

fn create_ammonia<'a>() -> ammonia::Builder<'a> {
  let mut ammonia_builder = ammonia::Builder::default();

  ammonia_builder
    .add_tags(["video", "button", "svg", "path", "rect"])
    .add_generic_attributes(["id", "align"])
    .add_tag_attributes("button", ["data-copy"])
    .add_tag_attributes(
      "svg",
      [
        "width",
        "height",
        "viewBox",
        "fill",
        "xmlns",
        "stroke",
        "stroke-width",
        "stroke-linecap",
        "stroke-linejoin",
      ],
    )
    .add_tag_attributes(
      "path",
      [
        "d",
        "fill",
        "fill-rule",
        "clip-rule",
        "stroke",
        "stroke-width",
        "stroke-linecap",
        "stroke-linejoin",
      ],
    )
    .add_tag_attributes("rect", ["x", "y", "width", "height", "fill"])
    .add_tag_attributes("video", ["src", "controls"])
    .add_allowed_classes("pre", ["highlight"])
    .add_allowed_classes("button", ["context_button"])
    .add_allowed_classes(
      "div",
      [
        "alert",
        "alert-note",
        "alert-tip",
        "alert-important",
        "alert-warning",
        "alert-caution",
      ],
    )
    .link_rel(Some("nofollow"))
    .url_relative(ammonia::UrlRelative::Custom(Box::new(
      AmmoniaRelativeUrlEvaluator(),
    )));

  ammonia_builder
}

struct AmmoniaRelativeUrlEvaluator();

impl<'b> ammonia::UrlRelativeEvaluate<'b> for AmmoniaRelativeUrlEvaluator {
  fn evaluate<'a>(&self, url: &'a str) -> Option<Cow<'a, str>> {
    URL_REWRITER.with(|url_rewriter| {
      if let Some(url_rewriter) = url_rewriter.borrow().as_ref().unwrap() {
        CURRENT_FILE.with(|current_file| {
          Some(
            url_rewriter(current_file.borrow().as_ref().unwrap().as_ref(), url)
              .into(),
          )
        })
      } else {
        Some(Cow::Borrowed(url))
      }
    })
  }
}

enum Alert {
  Note,
  Tip,
  Important,
  Warning,
  Caution,
}

fn match_node_value<'a>(
  arena: &'a Arena<AstNode<'a>>,
  node: &'a AstNode<'a>,
  options: &comrak::Options,
  plugins: &comrak::Plugins,
) {
  match &node.data.borrow().value {
    NodeValue::BlockQuote => {
      if let Some(paragraph_child) = node.first_child() {
        if paragraph_child.data.borrow().value == NodeValue::Paragraph {
          let alert = paragraph_child.first_child().and_then(|text_child| {
            if let NodeValue::Text(text) = &text_child.data.borrow().value {
              match text
                .split_once(' ')
                .map_or((text.as_str(), None), |(kind, title)| {
                  (kind, Some(title))
                }) {
                ("[!NOTE]", title) => {
                  Some((Alert::Note, title.unwrap_or("Note").to_string()))
                }
                ("[!TIP]", title) => {
                  Some((Alert::Tip, title.unwrap_or("Tip").to_string()))
                }
                ("[!IMPORTANT]", title) => Some((
                  Alert::Important,
                  title.unwrap_or("Important").to_string(),
                )),
                ("[!WARNING]", title) => {
                  Some((Alert::Warning, title.unwrap_or("Warning").to_string()))
                }
                ("[!CAUTION]", title) => {
                  Some((Alert::Caution, title.unwrap_or("Caution").to_string()))
                }
                _ => None,
              }
            } else {
              None
            }
          });

          if let Some((alert, title)) = alert {
            let start_col = node.data.borrow().sourcepos.start;

            let document = arena.alloc(AstNode::new(RefCell::new(Ast::new(
              NodeValue::Document,
              start_col,
            ))));

            let node_without_alert = arena.alloc(AstNode::new(RefCell::new(
              Ast::new(NodeValue::Paragraph, start_col),
            )));

            for child_node in paragraph_child.children().skip(1) {
              node_without_alert.append(child_node);
            }
            for child_node in node.children().skip(1) {
              node_without_alert.append(child_node);
            }

            document.append(node_without_alert);

            let html = render_node(document, options, plugins);

            let alert_title = match alert {
              Alert::Note => format!(
                "{}{title}",
                include_str!("./templates/icons/info-circle.svg")
              ),
              Alert::Tip => {
                format!("{}{title}", include_str!("./templates/icons/bulb.svg"))
              }
              Alert::Important => format!(
                "{}{title}",
                include_str!("./templates/icons/warning-message.svg")
              ),
              Alert::Warning => format!(
                "{}{title}",
                include_str!("./templates/icons/warning-triangle.svg")
              ),
              Alert::Caution => format!(
                "{}{title}",
                include_str!("./templates/icons/warning-octagon.svg")
              ),
            };

            let html = format!(
              r#"<div class="alert alert-{}"><div>{alert_title}</div><div>{html}</div></div>"#,
              match alert {
                Alert::Note => "note",
                Alert::Tip => "tip",
                Alert::Important => "important",
                Alert::Warning => "warning",
                Alert::Caution => "caution",
              }
            );

            let alert_node = arena.alloc(AstNode::new(RefCell::new(Ast::new(
              NodeValue::HtmlBlock(NodeHtmlBlock {
                block_type: 6,
                literal: html,
              }),
              start_col,
            ))));
            node.insert_before(alert_node);
            node.detach();
          }
        }
      }
    }
    NodeValue::Link(link) => {
      if link.url.ends_with(".mov") || link.url.ends_with(".mp4") {
        let start_col = node.data.borrow().sourcepos.start;

        let html = format!(r#"<video src="{}" controls></video>"#, link.url);

        let alert_node = arena.alloc(AstNode::new(RefCell::new(Ast::new(
          NodeValue::HtmlBlock(NodeHtmlBlock {
            block_type: 6,
            literal: html,
          }),
          start_col,
        ))));
        node.insert_before(alert_node);
        node.detach();
      }
    }
    _ => {}
  }
}

fn walk_node<'a>(
  arena: &'a Arena<AstNode<'a>>,
  node: &'a AstNode<'a>,
  options: &comrak::Options,
  plugins: &comrak::Plugins,
) {
  for child in node.children() {
    match_node_value(arena, child, options, plugins);
    walk_node(arena, child, options, plugins);
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

fn render_node<'a>(
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

  walk_node(&arena, root, &options, &Default::default());

  fn collect_text<'a>(node: &'a AstNode<'a>, output: &mut BufWriter<Vec<u8>>) {
    match node.data.borrow().value {
      NodeValue::Text(ref literal)
      | NodeValue::Code(comrak::nodes::NodeCode { ref literal, .. }) => {
        output.write_all(literal.as_bytes()).unwrap();
      }
      NodeValue::LineBreak | NodeValue::SoftBreak => {
        output.write_all(&[b' ']).unwrap()
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

pub type AmmoniaHook = Rc<dyn Fn(&mut ammonia::Builder)>;

pub fn create_renderer(
  syntax_highlighter: Option<
    Rc<dyn comrak::adapters::SyntaxHighlighterAdapter>,
  >,
  ammonia_hook: Option<AmmoniaHook>,
  url_rewriter: Option<URLRewriter>,
) -> super::jsdoc::MarkdownRenderer {
  let renderer = move |md: &str,
                       title_only: bool,
                       file_path: Option<ShortPath>,
                       anchorizer: super::jsdoc::Anchorizer|
        -> Option<String> {
    let mut options = comrak::Options::default();
    options.extension.autolink = true;
    options.extension.description_lists = true;
    options.extension.strikethrough = true;
    options.extension.superscript = true;
    options.extension.table = true;
    options.extension.tagfilter = true;
    options.extension.tasklist = true;
    options.render.unsafe_ = true; // its fine because we run ammonia afterwards

    let mut plugins = comrak::Plugins::default();
    let heading_adapter = ComrakHeadingAdapter(anchorizer);

    if !title_only {
      plugins.render.codefence_syntax_highlighter =
        syntax_highlighter.as_deref();
      plugins.render.heading_adapter = Some(&heading_adapter);
    }

    let class_name = if title_only {
      "markdown_summary"
    } else {
      "markdown"
    };

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
        walk_node(&arena, root, &options, &plugins);
        render_node(root, &options, &plugins)
      }
    };

    CURRENT_FILE.set(Some(file_path));
    URL_REWRITER.set(Some(url_rewriter.clone()));

    let mut ammonia = create_ammonia();

    if let Some(ammonia_hook) = ammonia_hook.clone() {
      ammonia_hook(&mut ammonia);
    }

    let html = format!(
      r#"<div class="{class_name}">{}</div>"#,
      ammonia.clean(&html)
    );

    CURRENT_FILE.set(None);
    URL_REWRITER.set(None);

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

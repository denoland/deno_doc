use super::render_context::RenderContext;
use super::util::*;
use crate::html::usage::UsagesCtx;
use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;
use crate::DocNodeKind;
use comrak::nodes::Ast;
use comrak::nodes::AstNode;
use comrak::nodes::NodeHtmlBlock;
use comrak::nodes::NodeValue;
use comrak::Arena;
use deno_ast::ModuleSpecifier;
use serde::Serialize;
use std::borrow::Cow;
use std::cell::RefCell;
use std::cmp::Ordering;

#[cfg(feature = "ammonia")]
use crate::html::comrak_adapters::URLRewriter;

lazy_static! {
  static ref JSDOC_LINK_RE: regex::Regex = regex::Regex::new(
    r"(?m)\{\s*@link(?P<modifier>code|plain)?\s+(?P<value>[^}]+)}"
  )
  .unwrap();
  static ref LINK_RE: regex::Regex =
    regex::Regex::new(r"(^\.{0,2}\/)|(^[A-Za-z]+:\S)").unwrap();
}

fn parse_links<'a>(md: &'a str, ctx: &RenderContext) -> Cow<'a, str> {
  JSDOC_LINK_RE.replace_all(md, |captures: &regex::Captures| {
    let code = captures
      .name("modifier")
      .map_or("plain", |modifier_match| modifier_match.as_str())
      == "code";
    let value = captures.name("value").unwrap().as_str();

    let (link, title) = if let Some((link, title)) =
      value.split_once('|').or_else(|| value.split_once(' '))
    {
      (link.trim(), title.trim())
    } else {
      (value, "")
    };

    let (title, link) = if let Some(href) = ctx.lookup_symbol_href(link) {
      let title = if title.is_empty() { link } else { title };

      (title, href)
    } else {
      (title, link.to_string())
    };

    if LINK_RE.is_match(&link) {
      if code {
        format!("[`{title}`]({link})")
      } else {
        format!("[{title}]({link})")
      }
    } else {
      let title = if !title.is_empty() {
        format!(" | {title}")
      } else {
        String::new()
      };

      if code {
        format!("`{link}`{title}")
      } else {
        format!("{link}{title}")
      }
    }
  })
}

fn split_markdown_title(md: &str) -> (Option<&str>, &str) {
  let newline = md.find("\n\n").unwrap_or(usize::MAX);
  let codeblock = md.find("```").unwrap_or(usize::MAX);

  let index = newline.min(codeblock).min(md.len());

  match md.split_at(index) {
    ("", body) => (None, body),
    (title, "") => (None, title),
    (title, body) => (Some(title), body),
  }
}

#[cfg(feature = "ammonia")]
struct AmmoniaRelativeUrlEvaluator {
  current_specifier: Option<ModuleSpecifier>,
  url_rewriter: URLRewriter,
}

#[cfg(feature = "ammonia")]
impl ammonia::UrlRelativeEvaluate for AmmoniaRelativeUrlEvaluator {
  fn evaluate<'a>(&self, url: &'a str) -> Option<Cow<'a, str>> {
    Some((self.url_rewriter)(self.current_specifier.as_ref(), url).into())
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
              match text.as_str() {
                "[!NOTE]" => Some(Alert::Note),
                "[!TIP]" => Some(Alert::Tip),
                "[!IMPORTANT]" => Some(Alert::Important),
                "[!WARNING]" => Some(Alert::Warning),
                "[!CAUTION]" => Some(Alert::Caution),
                _ => None,
              }
            } else {
              None
            }
          });

          if let Some(alert) = alert {
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

            document.append(node_without_alert);

            let html = render_node(document, options, plugins);

            let alert_title = match alert {
              Alert::Note => format!(
                "{}Note",
                include_str!("./templates/icons/info-circle.svg")
              ),
              Alert::Tip => {
                format!("{}Tip", include_str!("./templates/icons/bulb.svg"))
              }
              Alert::Important => format!(
                "{}Important",
                include_str!("./templates/icons/warning-message.svg")
              ),
              Alert::Warning => format!(
                "{}Warning",
                include_str!("./templates/icons/warning-triangle.svg")
              ),
              Alert::Caution => format!(
                "{}Caution",
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

fn render_node<'a>(
  node: &'a AstNode<'a>,
  options: &comrak::Options,
  plugins: &comrak::Plugins,
) -> String {
  let mut bw = std::io::BufWriter::new(Vec::new());
  comrak::format_html_with_plugins(node, options, &mut bw, plugins).unwrap();
  String::from_utf8(bw.into_inner().unwrap()).unwrap()
}

#[derive(Debug, Serialize, Clone)]
pub struct Markdown {
  pub html: String,
  pub toc: Option<String>,
}

pub fn markdown_to_html(
  render_ctx: &RenderContext,
  md: &str,
  summary: bool,
  render_toc: bool,
) -> Markdown {
  // TODO(bartlomieju): this should be initialized only once
  let mut options = comrak::Options::default();
  options.extension.autolink = true;
  options.extension.description_lists = true;
  options.extension.strikethrough = true;
  options.extension.superscript = true;
  options.extension.table = true;
  options.extension.tagfilter = true;
  options.extension.tasklist = true;
  #[cfg(not(feature = "ammonia"))]
  {
    options.render.escape = true;
  }
  #[cfg(feature = "ammonia")]
  {
    options.render.unsafe_ = true; // its fine because we run ammonia afterwards
  }

  let mut plugins = comrak::Plugins::default();
  plugins.render.codefence_syntax_highlighter =
    Some(&render_ctx.ctx.highlight_adapter);
  let heading_adapter =
    crate::html::comrak_adapters::HeadingToCAdapter::default();
  plugins.render.heading_adapter = Some(&heading_adapter);

  let md = parse_links(md, render_ctx);

  let md = if summary {
    let (title, body) = split_markdown_title(md.as_ref());
    title.unwrap_or(body)
  } else {
    md.as_ref()
  };

  let class_name = if summary {
    "markdown_summary"
  } else {
    "markdown"
  };

  let mut html = {
    let arena = Arena::new();
    let root = comrak::parse_document(&arena, md, &options);

    walk_node(&arena, root, &options, &plugins);

    render_node(root, &options, &plugins)
  };

  #[cfg(feature = "ammonia")]
  {
    let mut ammonia_builder = ammonia::Builder::default();

    ammonia_builder
      .add_tags(["video", "button", "svg", "path"])
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
      .url_relative(render_ctx.ctx.url_rewriter.as_ref().map_or(
        ammonia::UrlRelative::PassThrough,
        |url_rewriter| {
          ammonia::UrlRelative::Custom(Box::new(AmmoniaRelativeUrlEvaluator {
            current_specifier: render_ctx.get_current_specifier().cloned(),
            url_rewriter: url_rewriter.clone(),
          }))
        },
      ));

    #[cfg(feature = "syntect")]
    ammonia_builder.add_tag_attributes("span", ["style"]);

    #[cfg(feature = "tree-sitter")]
    ammonia_builder.add_allowed_classes("span", super::tree_sitter::CLASSES);

    html = ammonia_builder.clean(&html).to_string();
  }

  let toc = if render_toc {
    let toc = heading_adapter.into_toc();

    if toc.is_empty() {
      None
    } else {
      let mut toc_content = vec![String::from(r#"<nav class="toc"><ul>"#)];

      let mut current_level = 1;

      for (level, heading, anchor) in toc {
        match current_level.cmp(&level) {
          Ordering::Equal => {}
          Ordering::Less => {
            toc_content.push(r#"<li><ul>"#.to_string());
            current_level = level;
          }
          Ordering::Greater => {
            toc_content.push("</ul></li>".to_string());
            current_level = level;
          }
        }

        toc_content.push(format!(
          r##"<li><a href="#{anchor}" title="{heading}">{heading}</a></li>"##
        ));
      }

      toc_content.push(String::from("</ul></nav>"));

      Some(toc_content.join(""))
    }
  } else {
    None
  };

  Markdown {
    html: format!(r#"<div class="{class_name} flex-1">{html}</div>"#),
    toc,
  }
}

pub(crate) fn render_markdown_summary(
  render_ctx: &RenderContext,
  md: &str,
) -> String {
  markdown_to_html(render_ctx, md, true, false).html
}

pub(crate) fn render_markdown(render_ctx: &RenderContext, md: &str) -> String {
  markdown_to_html(render_ctx, md, false, false).html
}

pub(crate) fn jsdoc_body_to_html(
  ctx: &RenderContext,
  js_doc: &JsDoc,
  summary: bool,
) -> Option<String> {
  if let Some(doc) = js_doc.doc.as_deref() {
    if doc.is_empty() {
      None
    } else {
      Some(markdown_to_html(ctx, doc, summary, false).html)
    }
  } else {
    None
  }
}

pub(crate) fn jsdoc_examples(
  ctx: &RenderContext,
  js_doc: &JsDoc,
) -> Option<SectionCtx> {
  let mut i = 0;

  let examples = js_doc
    .tags
    .iter()
    .filter_map(|tag| {
      if let JsDocTag::Example { doc } = tag {
        let example = ExampleCtx::new(ctx, doc, i);
        i += 1;
        Some(example)
      } else {
        None
      }
    })
    .collect::<Vec<ExampleCtx>>();

  if !examples.is_empty() {
    Some(SectionCtx {
      title: "Examples".to_string(),
      content: SectionContentCtx::Example(examples),
    })
  } else {
    None
  }
}

#[derive(Debug, Serialize, Clone)]
pub struct ExampleCtx {
  anchor: AnchorCtx,
  id: String,
  markdown_title: String,
  markdown_body: String,
}

impl ExampleCtx {
  pub const TEMPLATE: &'static str = "example";

  pub fn new(render_ctx: &RenderContext, example: &str, i: usize) -> Self {
    let id = name_to_id("example", &i.to_string());

    let (maybe_title, body) = split_markdown_title(example);
    let title = if let Some(title) = maybe_title {
      title.to_string()
    } else {
      format!("Example {}", i + 1)
    };

    let markdown_title = render_markdown_summary(render_ctx, &title);
    let markdown_body = render_markdown(render_ctx, body);

    ExampleCtx {
      anchor: AnchorCtx { id: id.to_string() },
      id: id.to_string(),
      markdown_title,
      markdown_body,
    }
  }
}

#[derive(Debug, Serialize, Clone, Default)]
pub struct ModuleDocCtx {
  pub title: Option<String>,
  pub deprecated: Option<String>,
  pub usages: Option<UsagesCtx>,
  pub toc: Option<String>,
  pub sections: super::SymbolContentCtx,
}

impl ModuleDocCtx {
  pub const TEMPLATE: &'static str = "module_doc";

  pub fn new(
    render_ctx: &RenderContext,
    specifier: &ModuleSpecifier,
    doc_nodes_by_url: &super::ContextDocNodesByUrl,
  ) -> Self {
    let module_doc_nodes = doc_nodes_by_url.get(specifier).unwrap();

    let title = if !render_ctx.ctx.hide_module_doc_title {
      Some(render_ctx.ctx.url_to_short_path(specifier).to_name())
    } else {
      None
    };

    let mut sections = Vec::with_capacity(7);

    let (deprecated, html, toc) = if let Some(node) = module_doc_nodes
      .iter()
      .find(|n| n.kind == DocNodeKind::ModuleDoc)
    {
      let deprecated = node.js_doc.tags.iter().find_map(|tag| {
        if let JsDocTag::Deprecated { doc } = tag {
          Some(doc.to_owned().unwrap_or_default())
        } else {
          None
        }
      });

      if let Some(examples) = jsdoc_examples(render_ctx, &node.js_doc) {
        sections.push(examples);
      }

      let (html, toc) = if let Some(markdown) = node
        .js_doc
        .doc
        .as_ref()
        .map(|doc| markdown_to_html(render_ctx, doc, false, true))
      {
        (Some(markdown.html), markdown.toc)
      } else {
        (None, None)
      };

      (deprecated, html, toc)
    } else {
      (None, None, None)
    };

    if !render_ctx
      .ctx
      .main_entrypoint
      .as_ref()
      .is_some_and(|main_entrypoint| main_entrypoint == specifier)
    {
      let partitions_by_kind =
        super::partition::partition_nodes_by_kind(module_doc_nodes, true);

      sections.extend(super::namespace::render_namespace(
        render_ctx,
        partitions_by_kind,
      ));
    }

    Self {
      title,
      deprecated,
      usages: UsagesCtx::new(render_ctx, &[]),
      toc,
      sections: super::SymbolContentCtx {
        id: "module_doc".to_string(),
        docs: html,
        sections,
      },
    }
  }
}

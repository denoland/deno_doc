use super::render_context::RenderContext;
use super::util::*;
use crate::html::usage::UsagesCtx;
use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;
use crate::DocNode;
use crate::DocNodeKind;
use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use serde::Serialize;
use std::borrow::Cow;
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

  let html = comrak::markdown_to_html_with_plugins(md, &options, &plugins);

  #[cfg(feature = "ammonia")]
  let html = {
    let mut ammonia_builder = ammonia::Builder::default();

    ammonia_builder
      .add_tags(["video"])
      .add_generic_attributes(["id"])
      .add_allowed_classes("pre", ["highlight"])
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

    ammonia_builder.clean(&html).to_string()
  };

  let toc = if render_toc {
    let toc = heading_adapter.into_toc();

    if toc.is_empty() {
      None
    } else {
      let mut toc_content = vec![String::from(
        r#"<ul class="space-y-2 block overflow-y-auto h-full">"#,
      )];

      let mut current_level = 1;

      for (level, heading, anchor) in toc {
        match current_level.cmp(&level) {
          Ordering::Equal => {}
          Ordering::Less => {
            toc_content.push(r#"<li><ul class="ml-4 space-y-2">"#.to_string());
            current_level = level;
          }
          Ordering::Greater => {
            toc_content.push("</ul></li>".to_string());
            current_level = level;
          }
        }

        toc_content.push(format!(
          r##"<li><a class="hover:underline block overflow-x-hidden whitespace-nowrap text-ellipsis" href="#{anchor}" title="{heading}">{heading}</a></li>"##
        ));
      }

      toc_content.push(String::from("</ul>"));

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
        doc.as_ref().map(|doc| {
          let example = ExampleCtx::new(ctx, doc, i);
          i += 1;
          example
        })
      } else {
        None
      }
    })
    .collect::<Vec<ExampleCtx>>();

  if !examples.is_empty() {
    Some(SectionCtx {
      title: "Examples",
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
  pub fn new(
    render_ctx: &RenderContext,
    specifier: &ModuleSpecifier,
    doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNode>>,
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
      let module_doc_nodes_with_context = module_doc_nodes
        .iter()
        .map(|node| crate::html::DocNodeWithContext {
          origin: None,
          doc_node: node,
        })
        .collect::<Vec<_>>();

      let partitions_by_kind = super::namespace::partition_nodes_by_kind(
        &module_doc_nodes_with_context,
        true,
      );

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

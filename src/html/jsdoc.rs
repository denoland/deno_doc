use super::render_context::RenderContext;
use super::util::*;
use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;
use crate::DocNode;
use crate::DocNodeKind;
use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use serde::Serialize;
use std::cmp::Ordering;

lazy_static! {
  static ref JSDOC_LINK_RE: regex::Regex = regex::Regex::new(
    r"(?m)\{\s*@link(?P<modifier>code|plain)?\s+(?P<value>[^}]+)}"
  )
  .unwrap();
  static ref LINK_RE: regex::Regex =
    regex::Regex::new(r"(^\.{0,2}\/)|(^[A-Za-z]+:\S)").unwrap();
}

fn parse_links<'a>(
  md: &'a str,
  ctx: &RenderContext,
) -> std::borrow::Cow<'a, str> {
  JSDOC_LINK_RE.replace_all(md, |captures: &regex::Captures| {
    let code = captures
      .name("modifier")
      .map_or("plain", |modifier_match| modifier_match.as_str())
      == "code";
    let value = captures.name("value").unwrap().as_str();

    let (title, link) =
      if let Some(index) = value.find('|').or_else(|| value.find(' ')) {
        value.split_at(index)
      } else {
        ("", value)
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

fn render_markdown_inner(
  render_ctx: &RenderContext,
  md: &str,
  summary: bool,
  render_toc: bool,
) -> String {
  // TODO(bartlomieju): this should be initialized only once
  let mut options = comrak::Options::default();
  options.extension.autolink = true;
  options.extension.description_lists = true;
  options.extension.strikethrough = true;
  options.extension.superscript = true;
  options.extension.table = true;
  options.extension.tagfilter = true;
  options.extension.tasklist = true;
  options.render.escape = true;

  let mut plugins = comrak::Plugins::default();

  plugins.render.codefence_syntax_highlighter =
    Some(&render_ctx.ctx.syntect_adapter);

  let heading_adapter =
    crate::html::comrak_adapters::HeadingToCAdapter::default();
  plugins.render.heading_adapter = Some(&heading_adapter);

  let md = if summary {
    let (title, body) = split_markdown_title(md);
    title.unwrap_or(body)
  } else {
    md
  };

  let class_name = if summary {
    "markdown_summary"
  } else {
    "markdown"
  };
  let html = comrak::markdown_to_html_with_plugins(
    &parse_links(md, render_ctx),
    &options,
    &plugins,
  );

  let mut markdown = format!(r#"<div class="{class_name}">{html}</div>"#);

  if render_toc {
    let toc = heading_adapter.get_toc();
    let mut toc_content =
      vec![String::from(r#"<ul class="space-y-2 sticky top-4 block">"#)];

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
        r##"<li><a class="hover:underline block overflow-hidden whitespace-nowrap text-ellipsis" href="#{anchor}" title="{heading}">{heading}</a></li>"##
      ));
    }

    toc_content.push(String::from("</ul>"));

    markdown = format!(
      r#"<div class="flex flex-row gap-7">
        {markdown}
        <nav class="flex-none max-w-64 text-sm">{}</nav>
      </div>"#,
      toc_content.join("")
    );
  }

  markdown
}

pub(crate) fn render_markdown_summary(
  render_ctx: &RenderContext,
  md: &str,
) -> String {
  render_markdown_inner(render_ctx, md, true, false)
}

pub(crate) fn render_markdown(render_ctx: &RenderContext, md: &str) -> String {
  render_markdown_inner(render_ctx, md, false, false)
}

pub(crate) fn render_markdown_with_toc(
  render_ctx: &RenderContext,
  md: &str,
) -> String {
  render_markdown_inner(render_ctx, md, false, true)
}

// TODO(bartlomieju): `render_examples` and `summary` are mutually exclusive,
// use an enum instead?
fn render_docs_inner(
  ctx: &RenderContext,
  js_doc: &JsDoc,
  render_examples: bool,
  summary: bool,
) -> (Option<String>, Option<SectionCtx>) {
  let md = if let Some(doc) = js_doc.doc.as_deref() {
    if doc.is_empty() {
      None
    } else {
      Some(render_markdown_inner(ctx, doc, summary, false))
    }
  } else {
    None
  };

  let examples = if render_examples {
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
  } else {
    None
  };

  (md, examples)
}

pub(crate) fn render_docs_summary(
  ctx: &RenderContext,
  js_doc: &JsDoc,
) -> Option<String> {
  let (docs, _examples) = render_docs_inner(ctx, js_doc, false, true);

  docs
}

pub(crate) fn render_docs_with_examples(
  ctx: &RenderContext,
  js_doc: &JsDoc,
) -> (Option<String>, Option<SectionCtx>) {
  render_docs_inner(ctx, js_doc, true, false)
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

    // TODO: icons
    ExampleCtx {
      anchor: AnchorCtx { id: id.to_string() },
      id: id.to_string(),
      markdown_title,
      markdown_body,
    }
  }
}

#[derive(Debug, Serialize, Clone)]
pub struct ModuleDocCtx {
  title: Option<String>,
  docs: String,
}

impl ModuleDocCtx {
  pub fn new(
    render_ctx: &RenderContext,
    specifier: Option<&ModuleSpecifier>,
    doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNode>>,
  ) -> Option<Self> {
    if let Some(main_entrypoint) = specifier {
      let module_doc_nodes = doc_nodes_by_url.get(main_entrypoint).unwrap();

      let docs = module_doc_nodes
        .iter()
        .find(|n| n.kind == DocNodeKind::ModuleDoc);

      docs
        .and_then(|node| node.js_doc.doc.as_ref())
        .map(|docs_md| {
          let rendered_docs = render_markdown_with_toc(render_ctx, docs_md);

          Self {
            title: (!render_ctx.ctx.hide_module_doc_title).then(|| {
              super::short_path_to_name(
                &render_ctx.ctx.url_to_short_path(main_entrypoint),
              )
            }),
            docs: rendered_docs,
          }
        })
    } else {
      None
    }
  }
}

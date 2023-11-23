use super::util::*;
use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;
use serde::Serialize;
use serde_json::json;

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
  md: &str,
  render_ctx: &RenderContext,
  summary: bool,
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

  // TODO(@crowlKats): codeblock highlighting

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
  let html = comrak::markdown_to_html(&parse_links(md, render_ctx), &options);
  format!(r#"<div class="{class_name}">{html}</div>"#,)
}

pub(crate) fn render_markdown_summary(
  md: &str,
  render_ctx: &RenderContext,
) -> String {
  render_markdown_inner(md, render_ctx, true)
}

pub(crate) fn render_markdown(md: &str, render_ctx: &RenderContext) -> String {
  render_markdown_inner(md, render_ctx, false)
}

// TODO(bartlomieju): move to a separate anchor.html module
#[derive(Serialize)]
struct AnchorRenderCtx {
  href: String,
}

#[derive(Serialize)]
struct ExampleRenderCtx {
  anchor: AnchorRenderCtx,
  id: String,
  markdown_title: String,
  markdown_body: String,
}

// TODO(bartlomieju): `render_examples` and `summary` are mutually exclusive,
// use an enum instead?
fn render_docs_inner(
  ctx: &RenderContext,
  js_doc: &JsDoc,
  render_examples: bool,
  summary: bool,
) -> String {
  let mut content_parts = vec![];

  if let Some(doc) = js_doc.doc.as_deref() {
    content_parts.push(render_markdown_inner(doc, ctx, summary));
  }

  if render_examples {
    let mut i = 0;

    let examples = js_doc
      .tags
      .iter()
      .filter_map(|tag| {
        if let JsDocTag::Example { doc } = tag {
          doc.as_ref().map(|doc| {
            let example = render_example(ctx, doc, i);
            i += 1;
            example
          })
        } else {
          None
        }
      })
      .collect::<Vec<String>>();

    if !examples.is_empty() {
      let s = ctx.render(
        "section.html",
        &json!({
          "title": "Examples",
          "content": &examples.join(""),
        }),
      );
      content_parts.push(s);
    }
  }

  content_parts.join("")
}

pub(crate) fn render_docs_summary(
  ctx: &RenderContext,
  js_doc: &JsDoc,
) -> String {
  render_docs_inner(ctx, js_doc, false, true)
}

pub(crate) fn render_docs_with_examples(
  ctx: &RenderContext,
  js_doc: &JsDoc,
) -> String {
  render_docs_inner(ctx, js_doc, true, false)
}

fn get_example_render_ctx(
  example: &str,
  i: usize,
  render_ctx: &RenderContext,
) -> ExampleRenderCtx {
  let id = name_to_id("example", &i.to_string());

  let (maybe_title, body) = split_markdown_title(example);
  let title = if let Some(title) = maybe_title {
    title.to_string()
  } else {
    format!("Example {}", i + 1)
  };

  let markdown_title = render_markdown_summary(&title, render_ctx);
  let markdown_body = render_markdown(body, render_ctx);

  // TODO: icons
  ExampleRenderCtx {
    anchor: AnchorRenderCtx {
      href: id.to_string(),
    },
    id: id.to_string(),
    markdown_title,
    markdown_body,
  }
}

fn render_example(ctx: &RenderContext, example: &str, i: usize) -> String {
  let example_render_ctx = get_example_render_ctx(example, i, ctx);
  // TODO: icons
  ctx.render("example.html", &example_render_ctx)
}

pub(crate) fn render_doc_entry(
  ctx: &RenderContext,
  id: &str,
  name: &str,
  content: &str,
  jsdoc: Option<&str>,
) -> String {
  let maybe_jsdoc = jsdoc
    .map(|doc| render_markdown(doc, ctx))
    .unwrap_or_default();

  // TODO: sourceHref
  ctx.render(
    "doc_entry.html",
    &json!({
      "id": id,
      "name": name,
      "content": content,
      "anchor": {
        "href": id
      },
      "jsdoc": maybe_jsdoc,
    }),
  )
}

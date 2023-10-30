use super::util::*;
use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;

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

pub fn markdown_to_html(
  md: &str,
  summary: bool,
  ctx: &RenderContext,
) -> String {
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

  let html = comrak::markdown_to_html(&parse_links(md, ctx), &options);
  format!(
    r#"<div class="{}">{html}</div>"#,
    if summary {
      "markdown_summary"
    } else {
      "markdown"
    }
  )
}

pub fn render_docs(
  js_doc: &JsDoc,
  render_examples: bool,
  summary: bool,
  ctx: &RenderContext,
) -> String {
  let mut doc = if let Some(doc) = js_doc.doc.as_deref() {
    markdown_to_html(doc, summary, ctx)
  } else {
    "".to_string()
  };

  if render_examples {
    let mut i = 0;

    let examples = js_doc
      .tags
      .iter()
      .filter_map(|tag| {
        if let JsDocTag::Example { doc } = tag {
          doc.as_ref().map(|doc| {
            let example = render_example(doc, i, ctx);
            i += 1;
            example
          })
        } else {
          None
        }
      })
      .collect::<Vec<String>>();

    if !examples.is_empty() {
      doc.push_str(&section("Examples", &examples.join("")));
    }
  }

  doc
}

fn render_example(example: &str, i: usize, ctx: &RenderContext) -> String {
  let id = name_to_id("example", &i.to_string());

  let (title, body) = split_markdown_title(example);

  // TODO: icons

  format!(
    r#"<div class="example">{}<details id={id}><summary>{}</summary>{}</details></div>"#,
    anchor(&id),
    markdown_to_html(
      &title.map_or_else(
        || format!("Example {}", i + 1),
        |summary| summary.to_string()
      ),
      true,
      ctx,
    ),
    markdown_to_html(body, false, ctx),
  )
}

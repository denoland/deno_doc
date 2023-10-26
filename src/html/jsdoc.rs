use super::util::*;
use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;

pub fn markdown_to_html(md: &str, summary: bool) -> String {
  let mut extension_options = comrak::ExtensionOptions::default();
  extension_options.autolink = true;
  extension_options.description_lists = true;
  extension_options.strikethrough = true;
  extension_options.superscript = true;
  extension_options.table = true;
  extension_options.tagfilter = true;
  extension_options.tasklist = true;

  // TODO(@crowlKats): codeblock highlighting, link parsing

  let md = if summary {
    let (title, body) = split_markdown_title(md);
    title.unwrap_or(body)
  } else {
    md
  };

  let html = comrak::markdown_to_html(
    md,
    &comrak::Options {
      extension: extension_options,
      parse: Default::default(),
      render: Default::default(),
    },
  );
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
) -> String {
  let mut doc = if let Some(doc) = js_doc.doc.as_deref() {
    markdown_to_html(doc, summary)
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
            let example = render_example(doc, i);
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

fn render_example(example: &str, i: usize) -> String {
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
      true
    ),
    markdown_to_html(body, false),
  )
}

pub fn get_default_value(js_doc: &JsDoc) -> Option<String> {
  js_doc.tags.iter().find_map(|tag| {
    if let JsDocTag::Default { value, .. } = tag {
      // TODO: font-normal
      Some(format!(
        r#"<span><span class="font-normal"> = </span>{value}</span>"#
      ))
    } else {
      None
    }
  })
}

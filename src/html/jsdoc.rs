use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;

pub fn markdown_to_html(md: &str) -> String {
  let mut extension_options = comrak::ExtensionOptions::default();
  extension_options.autolink = true;
  extension_options.description_lists = true;
  extension_options.strikethrough = true;
  extension_options.superscript = true;
  extension_options.table = true;
  extension_options.tagfilter = true;
  extension_options.tasklist = true;

  // TODO(@crowlKats): codeblock highlighting, link parsing

  let html = comrak::markdown_to_html(
    md,
    &comrak::Options {
      extension: extension_options,
      parse: Default::default(),
      render: Default::default(),
    },
  );
  format!(r#"<div class="jsdoc">{html}</div>"#)
}

pub fn render_docs(js_doc: &JsDoc) -> String {
  if let Some(doc) = js_doc.doc.as_deref() {
    markdown_to_html(doc)
  } else {
    "".to_string()
  }
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

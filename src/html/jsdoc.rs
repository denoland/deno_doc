use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;

pub fn render_docs(js_doc: &JsDoc) -> String {
  if let Some(doc) = js_doc.doc.as_deref() {
    let mkdown = markdown::to_html(doc);
    mkdown
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

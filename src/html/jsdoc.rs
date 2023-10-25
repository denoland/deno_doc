use super::util::*;
use crate::js_doc::JsDoc;

pub fn render_docs(js_doc: &JsDoc) -> String {
  if let Some(doc) = js_doc.doc.as_deref() {
    let mkdown = markdown::to_html(doc);
    mkdown
  } else {
    "".to_string()
  }
}

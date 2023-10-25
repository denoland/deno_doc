use crate::js_doc::JsDoc;

lazy_static! {
  static ref TARGET_RE: regex::Regex = regex::Regex::new(r"\s*\* ?").unwrap();
}

pub fn name_to_id(kind: &str, name: &str) -> String {
  format!("{kind}_{}", TARGET_RE.replace_all(name, "_"))
}

fn section_title(title: &str) -> String {
  let id = TARGET_RE.replace_all(title, "_");

  format!(
    r##"<h2 class="section_title" id="{id}"><a href="#{id}" aria-label="Anchor">{title}</a></h2>"##
  )
}

pub fn section(title: &str, content: &str) -> String {
  format!(
    r#"<div>{}<div class="section">{}</div></div>"#,
    section_title(title),
    content,
  )
}

pub fn doc_entry(
  id: &str,
  name: &str,
  content: &str,
  jsdoc: Option<&str>,
) -> String {
  // TODO: jsdoc, sourceHref
  format!(
    r#"
    <div class="doc_item" id="{id}">
      {}
      <div class="doc_entry">
        {}
        <span class="doc_entry_children">
          <code>
            <span style="font-weight: 700;">{name}</span><span style="font-weight: 500;">{content}</span>
          </code>
        </span>

      </div>
    </div>
   "#,
    jsdoc.map(markdown::to_html).unwrap_or_default(),
    anchor(id),
  )
}

pub fn anchor(name: &str) -> String {
  // TODO: icon
  format!(
    r##"<a
      href="#{name}"
      class="anchor"
      aria-label="Anchor"
      tabIndex=-1
    >
      <div style="width: 14px; height: 14px; display: inline-block;">F</div>
    </a>"##
  )
}

lazy_static! {
  static ref TARGET_RE: regex::Regex = regex::Regex::new(r"\s*\* ?").unwrap();
}

pub fn name_to_id(kind: &str, name: &str) -> String {
  format!("{kind}_{}", TARGET_RE.replace_all(name, "_"))
}

fn section_title(title: &str) -> String {
  let id = TARGET_RE.replace_all(title, "_");

  format!(
    r#"<h2 class="section_title" id={id}><a href="\#{id}" aria-label="Anchor">{title}</a></h2>"#
  )
}

pub fn section(title: &str, content: &str) -> String {
  format!(
    r#"<div>{}<div class="section">{}</div></div>"#,
    section_title(title),
    content,
  )
}

pub fn doc_entry(id: &str, name: &str, content: &str) -> String {
  // TODO: jsdoc, sourceHref, font-mono
  format!(
    r#"
    <div class="doc_item" id="{id}">
      {}
      <div class="doc_entry">
        <span class="doc_entry_children">
        </span>

        <span class="font-mono">
          <span class="font-weight: 700;">{name}</span>
          <span style="font-weight: 500;">{content}</span>
        </span>
      </div>
    </div>
   "#,
    anchor(id),
  )
}

fn anchor(name: &str) -> String {
  // TODO: icon
  format!(
    r#"<a
      href="\#{name}"
      class="anchor"
      aria-label="Anchor"
      tabIndex=-1
    >
      F
    </a>"#
  )
}

// TODO: classes: section_title, section, doc_item, doc_entry, doc_entry_children, anchor

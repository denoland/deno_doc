use crate::DocNodeKind;
lazy_static! {
  static ref TARGET_RE: regex::Regex = regex::Regex::new(r"\s*\* ?").unwrap();
}

pub fn name_to_id(kind: &str, name: &str) -> String {
  format!("{kind}_{}", TARGET_RE.replace_all(name, "_"))
}

pub fn section_title(title: &str) -> String {
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
        <span class="doc_entry_children">
          <code>
            <span style="font-weight: 700;">{name}</span><span style="font-weight: 500;">{content}</span>
          </code>
        </span>
      </div>
      {}
    </div>
   "#,
    anchor(id),
    jsdoc
      .map(|doc| super::jsdoc::markdown_to_html(doc, false))
      .unwrap_or_default(),
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

pub struct RenderContext {
  pub additional_css: std::rc::Rc<std::cell::RefCell<String>>,
  pub namespace: Option<String>,
}

pub fn split_markdown_title(md: &str) -> (Option<&str>, &str) {
  let newline = md.find("\n\n").unwrap_or(usize::MAX);
  let codeblock = md.find("```").unwrap_or(usize::MAX);

  let index = newline.min(codeblock).min(md.len());

  match md.split_at(index) {
    ("", body) => (None, body),
    (title, "") => (None, title),
    (title, body) => (Some(title), body),
  }
}

pub fn doc_node_kind_icon(kind: DocNodeKind) -> String {
  let (char, title) = match kind {
    DocNodeKind::Function => ('f', "Function"),
    DocNodeKind::Variable => ('v', "Variable"),
    DocNodeKind::Class => ('c', "Class"),
    DocNodeKind::Enum => ('E', "Enum"),
    DocNodeKind::Interface => ('I', "Interface"),
    DocNodeKind::TypeAlias => ('T', "Type Alias"),
    DocNodeKind::Namespace => ('N', "Namespace"),
    DocNodeKind::ModuleDoc | DocNodeKind::Import => unimplemented!(),
  };

  format!(
    r#"<div class="symbol_kind kind_{kind:?}_text kind_{kind:?}_bg" title="{title}">{char}</div>"#
  )
}

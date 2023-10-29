use crate::DocNodeKind;
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

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
  ctx: &RenderContext,
) -> String {
  // TODO: sourceHref
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
      .map(|doc| super::jsdoc::markdown_to_html(doc, false, ctx))
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

#[derive(Debug, Clone)]
pub struct RenderContext {
  pub additional_css: Rc<RefCell<String>>,
  pub namespace: Option<String>,
  pub current_symbols: Rc<HashSet<Vec<String>>>,
  pub current_type_params: HashSet<String>,
}

impl RenderContext {
  pub fn lookup_symbol_href(&self, target_symbol: &str) -> Option<String> {
    if let Some(namespace) = &self.namespace {
      let mut parts = namespace
        .split('.')
        .map(String::from)
        .collect::<Vec<String>>();
      while !parts.is_empty() {
        let mut current_parts = parts.clone();
        current_parts.extend(target_symbol.split('.').map(String::from));

        if self.current_symbols.contains(&current_parts) {
          let backs = current_parts.iter().map(|_| "../").collect::<String>();

          return Some(format!("./{backs}{}.html", current_parts.join("/")));
        }

        // TODO: global symbol handling

        parts.pop();
      }
    }

    let split_symbol = target_symbol
      .split('.')
      .map(String::from)
      .collect::<Vec<String>>();

    if self.current_symbols.contains(&split_symbol) {
      let backs = if let Some(namespace) = &self.namespace {
        namespace.split('.').map(|_| "../").collect::<String>()
      } else {
        String::new()
      };

      return Some(format!("./{backs}{}.html", split_symbol.join("/")));
    }

    // TODO: handle currentImports

    None
  }
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

  // TODO: already a template, dedupe
  format!(
    r#"<div class="symbol_kind kind_{kind:?}_text kind_{kind:?}_bg" title="{title}">{char}</div>"#
  )
}

#[derive(Debug, serde::Serialize)]
pub struct DocNodeKindCtx {
  kind: String,
  char: char,
  title: &'static str,
  title_plural: &'static str,
}

impl From<&DocNodeKind> for DocNodeKindCtx {
  fn from(kind: &DocNodeKind) -> Self {
    let (char, title, title_plural) = match kind {
      DocNodeKind::Function => ('f', "Function", "Functions"),
      DocNodeKind::Variable => ('v', "Variable", "Variables"),
      DocNodeKind::Class => ('c', "Class", "Classes"),
      DocNodeKind::Enum => ('E', "Enum", "Enums"),
      DocNodeKind::Interface => ('I', "Interface", "Interfaces"),
      DocNodeKind::TypeAlias => ('T', "Type Alias", "Type Aliases"),
      DocNodeKind::Namespace => ('N', "Namespace", "Namespaces"),
      DocNodeKind::ModuleDoc | DocNodeKind::Import => unimplemented!(),
    };

    Self {
      kind: format!("{kind:?}"),
      char,
      title,
      title_plural,
    }
  }
}

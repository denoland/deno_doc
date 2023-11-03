use crate::DocNodeKind;
use serde_json::json;
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use super::GenerateCtx;

lazy_static! {
  static ref TARGET_RE: regex::Regex = regex::Regex::new(r"\s*\* ?").unwrap();
}

pub fn name_to_id(kind: &str, name: &str) -> String {
  format!("{kind}_{}", TARGET_RE.replace_all(name, "_"))
}

// TODO(bartlomieju): make it a template
pub fn section_title(title: &str) -> String {
  // TODO(bartlomieju): this could be a TinyTemplate formatter
  let id = TARGET_RE.replace_all(title, "_");

  format!(
    r##"<h2 class="section_title" id="{id}"><a href="#{id}" aria-label="Anchor">{title}</a></h2>"##
  )
}

pub(super) fn render_doc_entry(
  ctx: &GenerateCtx,
  id: &str,
  name: &str,
  content: &str,
  jsdoc: Option<&str>,
  render_ctx: &RenderContext,
) -> String {
  let maybe_jsdoc = jsdoc
    .map(|doc| super::jsdoc::markdown_to_html(doc, false, render_ctx))
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

#[derive(Debug, Clone)]
pub struct RenderContext {
  additional_css: Rc<RefCell<String>>,
  current_symbols: Rc<HashSet<Vec<String>>>,
  namespace: Option<String>,
  current_type_params: HashSet<String>,
}

impl RenderContext {
  pub fn new(
    current_symbols: Rc<HashSet<Vec<String>>>,
    namespace: Option<String>,
  ) -> Self {
    Self {
      additional_css: Default::default(),
      current_type_params: Default::default(),
      namespace,
      current_symbols,
    }
  }

  pub fn with_current_type_params(
    &self,
    current_type_params: HashSet<String>,
  ) -> Self {
    Self {
      current_type_params,
      ..self.clone()
    }
  }

  pub fn with_namespace(&self, namespace: String) -> Self {
    Self {
      namespace: Some(namespace),
      ..self.clone()
    }
  }

  pub fn add_additional_css(&self, css: String) {
    self.additional_css.borrow_mut().push_str(&css);
  }

  pub fn take_additional_css(&self) -> String {
    let mut css = self.additional_css.borrow_mut();
    std::mem::replace(&mut css, "".to_string())
  }

  pub fn contains_type_param(&self, name: &str) -> bool {
    self.current_type_params.contains(name)
  }

  pub fn get_namespace(&self) -> Option<String> {
    self.namespace.clone()
  }

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

// TODO(bartlomieju): make it a template
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

#[derive(Debug, serde::Serialize, Clone)]
pub struct DocNodeKindCtx {
  pub kind: String,
  char: char,
  title: &'static str,
  title_lowercase: &'static str,
  title_plural: &'static str,
}

impl From<&DocNodeKind> for DocNodeKindCtx {
  fn from(kind: &DocNodeKind) -> Self {
    let (char, title, title_lowercase, title_plural) = match kind {
      DocNodeKind::Function => ('f', "Function", "function", "Functions"),
      DocNodeKind::Variable => ('v', "Variable", "variable", "Variables"),
      DocNodeKind::Class => ('c', "Class", "class", "Classes"),
      DocNodeKind::Enum => ('E', "Enum", "enum", "Enums"),
      DocNodeKind::Interface => ('I', "Interface", "interface", "Interfaces"),
      DocNodeKind::TypeAlias => {
        ('T', "Type Alias", "type alias", "Type Aliases")
      }
      DocNodeKind::Namespace => ('N', "Namespace", "namespace", "Namespaces"),
      DocNodeKind::ModuleDoc | DocNodeKind::Import => unimplemented!(),
    };

    Self {
      kind: format!("{kind:?}"),
      char,
      title,
      title_lowercase,
      title_plural,
    }
  }
}

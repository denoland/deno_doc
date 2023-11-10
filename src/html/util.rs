use crate::DocNodeKind;
use serde::Serialize;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use tinytemplate::TinyTemplate;

lazy_static! {
  static ref TARGET_RE: regex::Regex = regex::Regex::new(r"\s*\* ?").unwrap();
}

pub fn name_to_id(kind: &str, name: &str) -> String {
  format!("{kind}_{}", TARGET_RE.replace_all(name, "_"))
}

// TODO(bartlomieju): this could be a TinyTemplate formatter
pub fn title_to_id(title: &str) -> String {
  TARGET_RE.replace_all(title, "_").to_string()
}

/// A container to hold a list of all available symbols with their namespaces:
///
/// ["setTimeout"]
/// ["Deno", "read"]
/// ["Deno", "errors"]
/// ["Deno", "errors", "HttpError"]
#[derive(Clone)]
pub struct NamespacedSymbols(Rc<HashSet<Vec<String>>>);

impl NamespacedSymbols {
  pub fn new(doc_nodes: &[crate::DocNode]) -> Self {
    let symbols = Self::compute_namespaced_symbols(doc_nodes, &[]);
    Self(Rc::new(symbols))
  }

  fn compute_namespaced_symbols(
    doc_nodes: &[crate::DocNode],
    current_path: &[String],
  ) -> HashSet<Vec<String>> {
    let mut namespaced_symbols = HashSet::new();

    for doc_node in doc_nodes {
      if doc_node.kind == DocNodeKind::ModuleDoc {
        continue;
      }

      let mut name_path = current_path.to_vec();
      name_path.push(doc_node.name.clone());

      namespaced_symbols.insert(name_path.clone());

      if doc_node.kind == DocNodeKind::Namespace {
        let namespace_def = doc_node.namespace_def.as_ref().unwrap();
        namespaced_symbols.extend(Self::compute_namespaced_symbols(
          &namespace_def.elements,
          &name_path,
        ))
      }
    }

    namespaced_symbols
  }

  fn contains(&self, path: &[String]) -> bool {
    self.0.contains(path)
  }
}

#[derive(Clone, Default)]
pub struct NamespacedGlobalSymbols(Rc<HashMap<Vec<String>, String>>);

impl NamespacedGlobalSymbols {
  pub fn new(symbols: HashMap<Vec<String>, String>) -> Self {
    Self(Rc::new(symbols))
  }

  fn get(&self, path: &[String]) -> Option<&String> {
    self.0.get(path)
  }
}

pub type GlobalSymbolHrefResolver = Rc<dyn Fn(&[String], &String) -> String>;

#[derive(Clone)]
pub struct RenderContext<'ctx> {
  tt: Rc<TinyTemplate<'ctx>>,
  all_symbols: NamespacedSymbols,
  current_type_params: HashSet<String>,
  /// A vector of parts of the current namespace, eg. `vec!["Deno", "errors"]`.
  namespace_parts: Vec<String>,
  global_symbols: NamespacedGlobalSymbols,
  global_symbol_href_resolver: GlobalSymbolHrefResolver,
}

impl<'ctx> RenderContext<'ctx> {
  pub fn new(
    tt: Rc<TinyTemplate<'ctx>>,
    all_symbols: NamespacedSymbols,
    global_symbols: NamespacedGlobalSymbols,
    global_symbol_href_resolver: GlobalSymbolHrefResolver,
  ) -> Self {
    Self {
      tt,
      current_type_params: Default::default(),
      namespace_parts: vec![],
      all_symbols,
      global_symbols,
      global_symbol_href_resolver,
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

  pub fn with_namespace(&self, namespace_parts: Vec<String>) -> Self {
    Self {
      namespace_parts,
      ..self.clone()
    }
  }

  #[track_caller]
  pub fn render<Ctx>(&self, template: &str, context: &Ctx) -> String
  where
    Ctx: Serialize,
  {
    self.tt.render(template, context).unwrap()
  }

  pub fn contains_type_param(&self, name: &str) -> bool {
    self.current_type_params.contains(name)
  }

  pub fn get_namespace_parts(&self) -> Vec<String> {
    self.namespace_parts.clone()
  }

  pub fn lookup_symbol_href(&self, target_symbol: &str) -> Option<String> {
    let target_symbol_parts = target_symbol
      .split('.')
      .map(String::from)
      .collect::<Vec<_>>();

    if !self.namespace_parts.is_empty() {
      let mut parts = self.namespace_parts.clone();
      while !parts.is_empty() {
        let mut current_parts = parts.clone();
        current_parts.extend_from_slice(&target_symbol_parts);

        if self.all_symbols.contains(&current_parts) {
          return Some(format!("./{}.html", current_parts.join(".")));
        }

        parts.pop();
      }
    }

    if self.all_symbols.contains(&target_symbol_parts) {
      return Some(format!("./{}.html", target_symbol_parts.join(".")));
    }

    // TODO(crowlKats): handle currentImports

    if let Some(context) = self.global_symbols.get(&target_symbol_parts) {
      return Some((self.global_symbol_href_resolver)(
        &target_symbol_parts,
        context,
      ));
    }

    None
  }
}

#[derive(Debug, serde::Serialize, Clone)]
pub struct DocNodeKindCtx {
  pub kind: String,
  pub char: char,
  pub title: &'static str,
  pub title_lowercase: &'static str,
  pub title_plural: &'static str,
}

impl From<DocNodeKind> for DocNodeKindCtx {
  fn from(kind: DocNodeKind) -> Self {
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

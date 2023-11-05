use crate::DocNodeKind;
use serde::Serialize;
use std::cell::RefCell;
use std::collections::HashSet;
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

#[derive(Clone)]
pub struct RenderContext<'ctx> {
  additional_css: Rc<RefCell<String>>,
  current_symbols: Rc<HashSet<Vec<String>>>,
  namespace: Option<String>,
  current_type_params: HashSet<String>,
  tt: Rc<TinyTemplate<'ctx>>,
}

impl<'ctx> RenderContext<'ctx> {
  pub fn new(
    tt: Rc<TinyTemplate<'ctx>>,
    current_symbols: Rc<HashSet<Vec<String>>>,
    namespace: Option<String>,
  ) -> Self {
    Self {
      tt,
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

  #[track_caller]
  pub fn render<Ctx>(&self, template: &str, context: &Ctx) -> String
  where
    Ctx: Serialize,
  {
    self.tt.render(template, context).unwrap()
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

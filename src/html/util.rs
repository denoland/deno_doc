use crate::html::GenerateCtx;
use crate::html::UrlResolveKind;
use crate::DocNodeKind;
use deno_graph::ModuleSpecifier;
use serde::Serialize;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

lazy_static! {
  static ref TARGET_RE: regex::Regex = regex::Regex::new(r"\s*\* ?").unwrap();
}

pub(crate) fn name_to_id(kind: &str, name: &str) -> String {
  format!("{kind}_{}", TARGET_RE.replace_all(name, "_"))
}

// TODO(bartlomieju): this could be a TinyTemplate formatter
pub(crate) fn title_to_id(title: &str) -> String {
  TARGET_RE.replace_all(title, "_").to_string()
}

/// A container to hold a list of symbols with their namespaces:
///
/// ["setTimeout"]
/// ["Deno", "read"]
/// ["Deno", "errors"]
/// ["Deno", "errors", "HttpError"]
#[derive(Clone)]
struct NamespacedSymbols(Rc<HashSet<Vec<String>>>);

impl NamespacedSymbols {
  fn new(doc_nodes: &[crate::DocNode]) -> Self {
    let symbols = compute_namespaced_symbols(doc_nodes, &[]);
    Self(Rc::new(symbols))
  }

  fn contains(&self, path: &[String]) -> bool {
    self.0.contains(path)
  }
}

pub fn compute_namespaced_symbols(
  doc_nodes: &[crate::DocNode],
  current_path: &[String],
) -> HashSet<Vec<String>> {
  let mut namespaced_symbols = HashSet::new();

  for doc_node in doc_nodes {
    if doc_node.kind == DocNodeKind::ModuleDoc
      || doc_node.declaration_kind != crate::node::DeclarationKind::Export
    {
      continue;
    }
    // TODO: handle export aliasing

    let mut name_path = current_path.to_vec();
    name_path.push(doc_node.name.clone());

    namespaced_symbols.insert(name_path.clone());

    if doc_node.kind == DocNodeKind::Namespace {
      let namespace_def = doc_node.namespace_def.as_ref().unwrap();
      namespaced_symbols.extend(compute_namespaced_symbols(
        &namespace_def.elements,
        &name_path,
      ))
    }
  }

  namespaced_symbols
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

pub type ImportHrefResolver = Rc<dyn Fn(&[String], &String) -> Option<String>>;

#[derive(Clone)]
pub struct RenderContext<'ctx> {
  pub(crate) ctx: &'ctx GenerateCtx<'ctx>,
  current_exports: NamespacedSymbols,
  current_imports: HashMap<String, String>,
  current_type_params: HashSet<String>,
  current_resolve: UrlResolveKind<'ctx>,
  current_specifier: Option<&'ctx ModuleSpecifier>,
  /// A vector of parts of the current namespace, eg. `vec!["Deno", "errors"]`.
  namespace_parts: Vec<String>,
}

impl<'ctx> RenderContext<'ctx> {
  pub fn new(
    ctx: &'ctx GenerateCtx<'ctx>,
    doc_nodes: &[crate::DocNode],
    current_resolve: UrlResolveKind<'ctx>,
    current_specifier: Option<&'ctx ModuleSpecifier>,
  ) -> Self {
    Self {
      ctx,
      current_exports: NamespacedSymbols::new(doc_nodes),
      current_imports: get_current_imports(doc_nodes),
      current_type_params: Default::default(),
      current_resolve,
      current_specifier,
      namespace_parts: vec![],
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

  pub fn with_current_resolve(
    &self,
    current_resolve: UrlResolveKind<'ctx>,
  ) -> Self {
    Self {
      current_resolve,
      ..self.clone()
    }
  }

  pub fn with_current_specifier(
    &self,
    current_specifier: Option<&'ctx ModuleSpecifier>,
  ) -> Self {
    Self {
      current_specifier,
      ..self.clone()
    }
  }

  #[track_caller]
  pub fn render<Ctx>(&self, template: &str, context: &Ctx) -> String
  where
    Ctx: Serialize,
  {
    self.ctx.tt.render(template, context).unwrap()
  }

  pub fn contains_type_param(&self, name: &str) -> bool {
    self.current_type_params.contains(name)
  }

  pub fn get_namespace_parts(&self) -> Vec<String> {
    self.namespace_parts.clone()
  }

  pub fn get_current_resolve(&self) -> UrlResolveKind {
    self.current_resolve
  }

  pub fn get_current_specifier(&self) -> Option<&ModuleSpecifier> {
    self.current_specifier
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

        if self.current_exports.contains(&current_parts) {
          return Some((self.ctx.url_resolver)(
            self.get_current_resolve(),
            UrlResolveKind::Symbol {
              file: self.get_current_resolve().get_file().unwrap(),
              symbol: &current_parts.join("."),
            },
          ));
        }

        parts.pop();
      }
    }

    if self.current_exports.contains(&target_symbol_parts) {
      return Some((self.ctx.url_resolver)(
        self.get_current_resolve(),
        UrlResolveKind::Symbol {
          file: self.get_current_resolve().get_file().unwrap_or_default(), // TODO
          symbol: &target_symbol_parts.join("."),
        },
      ));
    }

    if let Some(src) = self.current_imports.get(target_symbol) {
      if let Ok(module_specifier) = ModuleSpecifier::parse(src) {
        if self.ctx.specifiers.contains(&module_specifier) {
          return Some((self.ctx.url_resolver)(
            self.get_current_resolve(),
            UrlResolveKind::Symbol {
              file: &self.ctx.url_to_short_path(&module_specifier),
              symbol: &target_symbol_parts.join("."),
            },
          ));
        }
      }

      return (self.ctx.import_href_resolver)(&target_symbol_parts, src);
    }

    if let Some(context) = self.ctx.global_symbols.get(&target_symbol_parts) {
      return Some((self.ctx.global_symbol_href_resolver)(
        &target_symbol_parts,
        context,
      ));
    }

    None
  }

  pub fn get_breadcrumbs(&self) -> BreadcrumbsCtx {
    let parts = match self.current_resolve {
      UrlResolveKind::Root => vec![BreadcrumbCtx {
        name: "index".to_string(),
        href: "".to_string(),
        is_symbol: false,
        is_first_symbol: false,
        is_all_symbols_part: false,
      }],
      UrlResolveKind::AllSymbols => {
        vec![
          BreadcrumbCtx {
            name: "index".to_string(),
            href: (self.ctx.url_resolver)(
              self.current_resolve,
              UrlResolveKind::Root,
            ),
            is_symbol: false,
            is_first_symbol: false,
            is_all_symbols_part: false,
          },
          BreadcrumbCtx {
            name: "all symbols".to_string(),
            href: "".to_string(),
            is_symbol: false,
            is_first_symbol: false,
            is_all_symbols_part: true,
          },
        ]
      }
      UrlResolveKind::File(file) => {
        if self.current_specifier == self.ctx.main_entrypoint.as_ref() {
          vec![BreadcrumbCtx {
            name: "index".to_string(),
            href: "".to_string(),
            is_symbol: false,
            is_first_symbol: false,
            is_all_symbols_part: false,
          }]
        } else {
          vec![
            BreadcrumbCtx {
              name: "index".to_string(),
              href: (self.ctx.url_resolver)(
                self.current_resolve,
                UrlResolveKind::Root,
              ),
              is_symbol: false,
              is_first_symbol: false,
              is_all_symbols_part: false,
            },
            BreadcrumbCtx {
              name: super::short_path_to_name(file.to_string()),
              href: "".to_string(),
              is_symbol: false,
              is_first_symbol: false,
              is_all_symbols_part: false,
            },
          ]
        }
      }
      UrlResolveKind::Symbol { file, symbol } => {
        let mut parts = vec![BreadcrumbCtx {
          name: "index".to_string(),
          href: (self.ctx.url_resolver)(
            self.current_resolve,
            UrlResolveKind::Root,
          ),
          is_symbol: false,
          is_first_symbol: false,
          is_all_symbols_part: false,
        }];

        if self.current_specifier != self.ctx.main_entrypoint.as_ref() {
          parts.push(BreadcrumbCtx {
            name: super::short_path_to_name(file.to_string()),
            href: (self.ctx.url_resolver)(
              self.current_resolve,
              UrlResolveKind::File(file),
            ),
            is_symbol: false,
            is_first_symbol: false,
            is_all_symbols_part: false,
          });
        }

        let (_, symbol_parts) = symbol.split('.').enumerate().fold(
          (vec![], vec![]),
          |(mut symbol_parts, mut breadcrumbs), (i, symbol_part)| {
            symbol_parts.push(symbol_part);
            let breadcrumb = BreadcrumbCtx {
              name: symbol_part.to_string(),
              href: (self.ctx.url_resolver)(
                self.current_resolve,
                UrlResolveKind::Symbol {
                  file,
                  symbol: &symbol_parts.join("."),
                },
              ),
              is_symbol: true,
              is_first_symbol: i == 0,
              is_all_symbols_part: false,
            };
            breadcrumbs.push(breadcrumb);

            (symbol_parts, breadcrumbs)
          },
        );

        parts.extend(symbol_parts);

        parts
      }
    };

    BreadcrumbsCtx { parts }
  }
}

#[derive(Debug, Serialize, Clone)]
pub struct BreadcrumbCtx {
  name: String,
  href: String,
  is_symbol: bool,
  is_first_symbol: bool,
  is_all_symbols_part: bool,
}

#[derive(Debug, Serialize, Clone)]
pub struct BreadcrumbsCtx {
  parts: Vec<BreadcrumbCtx>,
}

#[derive(Debug, Serialize, Clone)]
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

fn get_current_imports(
  doc_nodes: &[crate::DocNode],
) -> HashMap<String, String> {
  let mut imports = HashMap::new();

  for doc_node in doc_nodes {
    if doc_node.kind == DocNodeKind::Import {
      let import_def = doc_node.import_def.as_ref().unwrap();
      // TODO: handle import aliasing
      if import_def.imported.as_ref() == Some(&doc_node.name) {
        imports.insert(doc_node.name.clone(), import_def.src.clone());
      }
    }
  }

  imports
}

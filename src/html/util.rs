use crate::html::DocNodeWithContext;
use crate::html::RenderContext;
use crate::html::ShortPath;
use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;
use crate::DocNodeKind;
use deno_ast::swc::ast::Accessibility;
use deno_ast::ModuleSpecifier;
use serde::Serialize;
use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

lazy_static! {
  static ref TARGET_RE: regex::Regex =
    regex::Regex::new(r"\s*\* ?|\.").unwrap();
}

pub(crate) fn name_to_id(kind: &str, name: &str) -> String {
  format!("{kind}_{}", TARGET_RE.replace_all(name, "_"))
}

/// A container to hold a list of symbols with their namespaces:
///
/// ["setTimeout"]
/// ["Deno", "read"]
/// ["Deno", "errors"]
/// ["Deno", "errors", "HttpError"]
#[derive(Clone, Debug)]
pub(crate) struct NamespacedSymbols(Rc<HashSet<Vec<String>>>);

impl NamespacedSymbols {
  pub(crate) fn new(doc_nodes: &[DocNodeWithContext]) -> Self {
    let symbols = compute_namespaced_symbols(doc_nodes.to_vec(), &[]);
    Self(Rc::new(symbols))
  }

  pub(crate) fn contains(&self, path: &[String]) -> bool {
    self.0.contains(path)
  }
}

pub fn compute_namespaced_symbols(
  doc_nodes: Vec<DocNodeWithContext>,
  current_path: &[String],
) -> HashSet<Vec<String>> {
  let mut namespaced_symbols = HashSet::new();

  for doc_node in doc_nodes {
    if doc_node.kind == DocNodeKind::ModuleDoc
      || doc_node.declaration_kind == crate::node::DeclarationKind::Private
    {
      continue;
    }
    // TODO: handle export aliasing

    let mut name_path = current_path.to_vec();
    name_path.push(doc_node.get_name().to_string());

    match doc_node.kind {
      DocNodeKind::Class => {
        let class_def = doc_node.class_def.as_ref().unwrap();

        namespaced_symbols.extend(class_def.methods.iter().map(|method| {
          let mut method_path = current_path.to_vec();
          method_path.extend(
            qualify_drilldown_name(
              doc_node.get_name(),
              &method.name,
              method.is_static,
            )
            .split('.')
            .map(|part| part.to_string()),
          );
          method_path
        }));

        namespaced_symbols.extend(class_def.properties.iter().map(
          |property| {
            let mut method_path = current_path.to_vec();
            method_path.extend(
              qualify_drilldown_name(
                doc_node.get_name(),
                &property.name,
                property.is_static,
              )
              .split('.')
              .map(|part| part.to_string()),
            );
            method_path
          },
        ));
      }
      DocNodeKind::Interface => {
        let interface_def = doc_node.interface_def.as_ref().unwrap();

        namespaced_symbols.extend(interface_def.methods.iter().map(|method| {
          let mut method_path = current_path.to_vec();
          method_path.extend(
            qualify_drilldown_name(doc_node.get_name(), &method.name, false)
              .split('.')
              .map(|part| part.to_string()),
          );
          method_path
        }));

        namespaced_symbols.extend(interface_def.properties.iter().map(
          |property| {
            let mut method_path = current_path.to_vec();
            method_path.extend(
              qualify_drilldown_name(
                doc_node.get_name(),
                &property.name,
                false,
              )
              .split('.')
              .map(|part| part.to_string()),
            );
            method_path
          },
        ));
      }
      _ => {}
    }

    namespaced_symbols.insert(name_path.clone());

    if doc_node.kind == DocNodeKind::Namespace {
      let namespace_def = doc_node.namespace_def.as_ref().unwrap();
      namespaced_symbols.extend(compute_namespaced_symbols(
        namespace_def
          .elements
          .iter()
          .map(|element| doc_node.create_child(element.clone()))
          .collect(),
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

  pub fn get(&self, path: &[String]) -> Option<&String> {
    self.0.get(path)
  }
}

/// Different current and target locations
#[derive(Debug, Clone, Copy)]
pub enum UrlResolveKind<'a> {
  Root,
  AllSymbols,
  File(&'a ShortPath),
  Symbol {
    file: &'a ShortPath,
    symbol: &'a str,
  },
}

impl UrlResolveKind<'_> {
  pub fn get_file(&self) -> Option<&ShortPath> {
    match self {
      UrlResolveKind::Root => None,
      UrlResolveKind::AllSymbols => None,
      UrlResolveKind::File(file) => Some(file),
      UrlResolveKind::Symbol { file, .. } => Some(file),
    }
  }
}

/// A trait used to define various functions used to resolve urls.
pub trait HrefResolver {
  fn resolve_path(
    &self,
    current: UrlResolveKind,
    target: UrlResolveKind,
  ) -> String {
    let backs = match current {
      UrlResolveKind::Symbol { file, .. } | UrlResolveKind::File(file) => "../"
        .repeat(if file.as_str() == "." {
          1
        } else {
          file.as_str().split('/').count() + 1
        }),
      UrlResolveKind::Root => String::new(),
      UrlResolveKind::AllSymbols => String::from("./"),
    };

    match target {
      UrlResolveKind::Root => backs,
      UrlResolveKind::AllSymbols => format!("{backs}./all_symbols.html"),
      UrlResolveKind::Symbol {
        file: target_file,
        symbol: target_symbol,
      } => {
        format!("{backs}./{}/~/{target_symbol}.html", target_file.as_str())
      }
      UrlResolveKind::File(target_file) => {
        format!("{backs}./{}/~/index.html", target_file.as_str())
      }
    }
  }

  /// Resolver for global symbols, like the Deno namespace or other built-ins
  fn resolve_global_symbol(&self, symbol: &[String]) -> Option<String>;

  /// Resolver for symbols from non-relative imports
  fn resolve_import_href(&self, symbol: &[String], src: &str)
    -> Option<String>;

  /// Resolve the URL used in "usage" blocks.
  fn resolve_usage(
    &self,
    current_specifier: &ModuleSpecifier,
    current_file: Option<&ShortPath>,
  ) -> Option<String>;

  /// Resolve the URL used in source code link buttons.
  fn resolve_source(&self, location: &crate::Location) -> Option<String>;
}

#[derive(Debug, Serialize, Clone)]
pub struct BreadcrumbCtx {
  pub name: String,
  pub href: String,
  pub is_symbol: bool,
  pub is_first_symbol: bool,
  pub is_all_symbols_part: bool,
}

#[derive(Debug, Serialize, Clone)]
pub struct BreadcrumbsCtx {
  pub parts: Vec<BreadcrumbCtx>,
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

#[derive(Debug, Serialize, Clone)]
pub struct AnchorCtx {
  pub id: String,
}

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "snake_case", tag = "kind", content = "content")]
pub enum SectionContentCtx {
  DocEntry(Vec<DocEntryCtx>),
  Example(Vec<super::jsdoc::ExampleCtx>),
  IndexSignature(Vec<super::symbols::class::IndexSignatureCtx>),
  NamespaceSection(Vec<super::namespace::NamespaceNodeCtx>),
}

#[derive(Debug, Serialize, Clone)]
pub struct SectionCtx {
  pub title: &'static str,
  pub content: SectionContentCtx,
}

#[derive(Debug, Serialize, Clone, Eq, PartialEq, Hash)]
#[serde(rename_all = "snake_case", tag = "kind", content = "value")]
pub enum Tag {
  New,
  Abstract,
  Deprecated,
  Writeonly,
  Readonly,
  Protected,
  Private,
  Optional,
  Permissions(Vec<String>),
  Other(String),
}

impl Tag {
  pub fn from_accessibility(
    accessibility: Option<Accessibility>,
  ) -> Option<Self> {
    match accessibility? {
      Accessibility::Public => None,
      Accessibility::Protected => Some(Tag::Protected),
      Accessibility::Private => Some(Tag::Private),
    }
  }

  pub fn from_js_doc(js_doc: &JsDoc) -> HashSet<Tag> {
    js_doc
      .tags
      .iter()
      .filter_map(|tag| match tag {
        JsDocTag::Deprecated { .. } => Some(Tag::Deprecated),
        _ => None,
      })
      .collect()
  }
}

#[derive(Debug, Serialize, Clone)]
pub struct DocEntryCtx {
  id: String,
  name: String,
  name_href: Option<String>,
  content: String,
  anchor: AnchorCtx,
  tags: HashSet<Tag>,
  js_doc: Option<String>,
  source_href: Option<String>,
}

impl DocEntryCtx {
  pub fn new(
    ctx: &RenderContext,
    id: &str,
    name: &str,
    name_href: Option<String>,
    content: &str,
    tags: HashSet<Tag>,
    jsdoc: Option<&str>,
    location: &crate::Location,
  ) -> Self {
    let maybe_jsdoc =
      jsdoc.map(|doc| crate::html::jsdoc::render_markdown(ctx, doc));
    let source_href = ctx.ctx.href_resolver.resolve_source(location);

    DocEntryCtx {
      id: id.to_string(),
      name: name.to_string(),
      name_href,
      content: content.to_string(),
      anchor: AnchorCtx { id: id.to_string() },
      tags,
      js_doc: maybe_jsdoc,
      source_href,
    }
  }
}

pub(crate) fn all_deprecated(nodes: &[&DocNodeWithContext]) -> bool {
  nodes.iter().all(|node| {
    node
      .js_doc
      .tags
      .iter()
      .any(|tag| matches!(tag, JsDocTag::Deprecated { .. }))
  })
}

pub(crate) fn qualify_drilldown_name(
  parent_name: &str,
  drilldown_name: &str,
  is_static: bool,
) -> String {
  format!(
    "{parent_name}{}.{drilldown_name}",
    if is_static { "" } else { ".prototype" },
  )
}

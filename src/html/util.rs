use crate::html::jsdoc::markdown_to_html;
use crate::html::jsdoc::MarkdownToHTMLOptions;
use crate::html::usage::UsagesCtx;
use crate::html::DocNodeKindWithDrilldown;
use crate::html::DocNodeWithContext;
use crate::html::RenderContext;
use crate::html::ShortPath;
use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;
use crate::DocNodeKind;
use deno_ast::swc::ast::Accessibility;
use deno_ast::swc::atoms::once_cell::sync::Lazy;
use regex::Regex;
use indexmap::IndexSet;
use serde::Serialize;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

lazy_static! {
  static ref TARGET_RE: regex::Regex =
    regex::Regex::new(r"\s*\* ?|\.").unwrap();
}

pub(crate) fn name_to_id(kind: &str, name: &str) -> String {
  format!(
    "{kind}_{}",
    html_escape::encode_safe(&TARGET_RE.replace_all(name, "_"))
  )
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
    let symbols = compute_namespaced_symbols(doc_nodes);
    Self(Rc::new(symbols))
  }

  pub(crate) fn contains(&self, path: &[String]) -> bool {
    self.0.contains(path)
  }
}

pub fn compute_namespaced_symbols(
  doc_nodes: &[DocNodeWithContext],
) -> HashSet<Vec<String>> {
  let mut namespaced_symbols = HashSet::new();

  for doc_node in doc_nodes {
    if doc_node.kind == DocNodeKind::ModuleDoc
      || doc_node.kind == DocNodeKind::Import
    {
      continue;
    }
    // TODO: handle export aliasing

    let name_path = Rc::new(doc_node.sub_qualifier());

    match doc_node.kind {
      DocNodeKind::Class => {
        let class_def = doc_node.class_def.as_ref().unwrap();

        namespaced_symbols.extend(class_def.methods.iter().map(|method| {
          let mut method_path = (*doc_node.ns_qualifiers).clone();
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
            let mut method_path = (*doc_node.ns_qualifiers).clone();
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
          let mut method_path = (*doc_node.ns_qualifiers).clone();
          method_path.extend(
            qualify_drilldown_name(doc_node.get_name(), &method.name, true)
              .split('.')
              .map(|part| part.to_string()),
          );
          method_path
        }));

        namespaced_symbols.extend(interface_def.properties.iter().map(
          |property| {
            let mut method_path = (*doc_node.ns_qualifiers).clone();
            method_path.extend(
              qualify_drilldown_name(doc_node.get_name(), &property.name, true)
                .split('.')
                .map(|part| part.to_string()),
            );
            method_path
          },
        ));
      }
      DocNodeKind::TypeAlias => {
        let type_alias_def = doc_node.type_alias_def.as_ref().unwrap();

        if let Some(type_literal) = type_alias_def.ts_type.type_literal.as_ref()
        {
          namespaced_symbols.extend(type_literal.methods.iter().map(
            |method| {
              let mut method_path = (*doc_node.ns_qualifiers).clone();
              method_path.extend(
                qualify_drilldown_name(doc_node.get_name(), &method.name, true)
                  .split('.')
                  .map(|part| part.to_string()),
              );
              method_path
            },
          ));

          namespaced_symbols.extend(type_literal.properties.iter().map(
            |property| {
              let mut method_path = (*doc_node.ns_qualifiers).clone();
              method_path.extend(
                qualify_drilldown_name(
                  doc_node.get_name(),
                  &property.name,
                  true,
                )
                .split('.')
                .map(|part| part.to_string()),
              );
              method_path
            },
          ));
        }
      }
      DocNodeKind::Variable => {
        let variable = doc_node.variable_def.as_ref().unwrap();

        if let Some(type_literal) = variable
          .ts_type
          .as_ref()
          .and_then(|ts_type| ts_type.type_literal.as_ref())
        {
          namespaced_symbols.extend(type_literal.methods.iter().map(
            |method| {
              let mut method_path = (*doc_node.ns_qualifiers).clone();
              method_path.extend(
                qualify_drilldown_name(doc_node.get_name(), &method.name, true)
                  .split('.')
                  .map(|part| part.to_string()),
              );
              method_path
            },
          ));

          namespaced_symbols.extend(type_literal.properties.iter().map(
            |property| {
              let mut method_path = (*doc_node.ns_qualifiers).clone();
              method_path.extend(
                qualify_drilldown_name(
                  doc_node.get_name(),
                  &property.name,
                  true,
                )
                .split('.')
                .map(|part| part.to_string()),
              );
              method_path
            },
          ));
        }
      }
      _ => {}
    }

    namespaced_symbols.insert((*name_path).clone());

    if doc_node.kind == DocNodeKind::Namespace {
      let namespace_def = doc_node.namespace_def.as_ref().unwrap();
      namespaced_symbols.extend(compute_namespaced_symbols(
        &namespace_def
          .elements
          .iter()
          .map(|element| {
            doc_node.create_namespace_child(element.clone(), name_path.clone())
          })
          .collect::<Vec<_>>(),
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
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum UrlResolveKind<'a> {
  Root,
  AllSymbols,
  Category(&'a str),
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
      UrlResolveKind::Category(_) => None,
      UrlResolveKind::File(file) => Some(file),
      UrlResolveKind::Symbol { file, .. } => Some(file),
    }
  }
}

pub fn href_path_resolve(
  current: UrlResolveKind,
  target: UrlResolveKind,
) -> String {
  let backs = match current {
    UrlResolveKind::File(file) => "../".repeat(if file.is_main {
      1
    } else {
      file.path.split('/').count()
    }),
    UrlResolveKind::Symbol { file, .. } => "../".repeat(if file.is_main {
      1
    } else {
      file.path.split('/').count() + 1
    }),
    UrlResolveKind::Root => String::new(),
    UrlResolveKind::AllSymbols => String::from("./"),
    UrlResolveKind::Category(_) => String::from("./"),
  };

  match target {
    UrlResolveKind::Root => backs,
    UrlResolveKind::File(target_file) if target_file.is_main => backs,
    UrlResolveKind::AllSymbols => format!("{backs}./all_symbols.html"),
    UrlResolveKind::Symbol {
      file: target_file,
      symbol: target_symbol,
      ..
    } => {
      format!("{backs}./{}/~/{target_symbol}.html", target_file.path)
    }
    UrlResolveKind::File(target_file) => {
      format!("{backs}./{}/index.html", target_file.path)
    }
    UrlResolveKind::Category(category) => {
      format!("{backs}./{}.html", slugify(category))
    }
  }
}

/// A trait used to define various functions used to resolve urls.
pub trait HrefResolver {
  fn resolve_path(
    &self,
    current: UrlResolveKind,
    target: UrlResolveKind,
  ) -> String;

  /// Resolver for global symbols, like the Deno namespace or other built-ins
  fn resolve_global_symbol(&self, symbol: &[String]) -> Option<String>;

  /// Resolver for symbols from non-relative imports
  fn resolve_import_href(&self, symbol: &[String], src: &str)
    -> Option<String>;

  /// Resolve the URL used in "usage" blocks.
  fn resolve_usage(&self, current_resolve: UrlResolveKind) -> Option<String>;

  /// Resolve the URL used in source code link buttons.
  fn resolve_source(&self, location: &crate::Location) -> Option<String>;
}

#[derive(Debug, Serialize, Clone)]
pub struct BreadcrumbCtx {
  pub name: String,
  pub href: String,
  pub is_symbol: bool,
  pub is_first_symbol: bool,
}

#[derive(Debug, Serialize, Clone)]
pub struct BreadcrumbsCtx {
  pub parts: Vec<BreadcrumbCtx>,
}

impl BreadcrumbsCtx {
  pub const TEMPLATE: &'static str = "breadcrumbs";
}

#[derive(Debug, Serialize, Clone, Eq, PartialEq, Hash)]
pub struct DocNodeKindCtx {
  pub kind: &'static str,
  pub char: char,
  pub title: &'static str,
  pub title_lowercase: &'static str,
  pub title_plural: &'static str,
}

impl From<DocNodeKindWithDrilldown> for DocNodeKindCtx {
  fn from(kind: DocNodeKindWithDrilldown) -> Self {
    let (char, kind, title, title_lowercase, title_plural) = match kind {
      DocNodeKindWithDrilldown::Property => {
        (' ', "Property", "Property", "property", "Properties")
      }
      DocNodeKindWithDrilldown::Method => {
        (' ', "Method", "Method", "method", "Methods")
      }
      DocNodeKindWithDrilldown::Other(DocNodeKind::Function) => {
        ('f', "Function", "Function", "function", "Functions")
      }
      DocNodeKindWithDrilldown::Other(DocNodeKind::Variable) => {
        ('v', "Variable", "Variable", "variable", "Variables")
      }
      DocNodeKindWithDrilldown::Other(DocNodeKind::Class) => {
        ('c', "Class", "Class", "class", "Classes")
      }
      DocNodeKindWithDrilldown::Other(DocNodeKind::Enum) => {
        ('E', "Enum", "Enum", "enum", "Enums")
      }
      DocNodeKindWithDrilldown::Other(DocNodeKind::Interface) => {
        ('I', "Interface", "Interface", "interface", "Interfaces")
      }
      DocNodeKindWithDrilldown::Other(DocNodeKind::TypeAlias) => {
        ('T', "TypeAlias", "Type Alias", "type alias", "Type Aliases")
      }
      DocNodeKindWithDrilldown::Other(DocNodeKind::Namespace) => {
        ('N', "Namespace", "Namespace", "namespace", "Namespaces")
      }
      DocNodeKindWithDrilldown::Other(DocNodeKind::ModuleDoc)
      | DocNodeKindWithDrilldown::Other(DocNodeKind::Import) => {
        unimplemented!()
      }
    };

    Self {
      kind,
      char,
      title,
      title_lowercase,
      title_plural,
    }
  }
}

#[derive(Debug, Serialize, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct AnchorCtx {
  pub id: String,
}

impl AnchorCtx {
  pub const TEMPLATE: &'static str = "anchor";
}

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "snake_case", tag = "kind", content = "content")]
pub enum SectionContentCtx {
  DocEntry(Vec<DocEntryCtx>),
  Example(Vec<super::jsdoc::ExampleCtx>),
  IndexSignature(Vec<super::symbols::class::IndexSignatureCtx>),
  NamespaceSection(Vec<super::namespace::NamespaceNodeCtx>),
}

#[derive(Debug, Serialize, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct SectionHeaderCtx {
  pub title: String,
  pub anchor: AnchorCtx,
  pub href: Option<String>,
  pub doc: Option<String>,
}

impl SectionHeaderCtx {
  pub fn new_for_namespace(
    render_ctx: &RenderContext,
    path: &ShortPath,
  ) -> Self {
    let module_doc_nodes = render_ctx.ctx.doc_nodes.get(path).unwrap();

    let doc = module_doc_nodes
      .iter()
      .find(|n| n.kind == DocNodeKind::ModuleDoc)
      .and_then(|node| node.js_doc.doc.as_ref())
      .and_then(|doc| {
        markdown_to_html(
          render_ctx,
          doc,
          MarkdownToHTMLOptions {
            summary: true,
            summary_prefer_title: true,
          },
        )
      });

    let title = path.display_name();

    SectionHeaderCtx {
      title: title.clone(),
      anchor: AnchorCtx { id: title },
      href: Some(render_ctx.ctx.href_resolver.resolve_path(
        render_ctx.get_current_resolve(),
        path.as_resolve_kind(),
      )),
      doc,
    }
  }
}

#[derive(Debug, Serialize, Clone)]
pub struct SectionCtx {
  pub header: SectionHeaderCtx,
  pub content: SectionContentCtx,
}

impl SectionCtx {
  pub const TEMPLATE: &'static str = "section";

  pub fn new(
    render_context: &RenderContext,
    title: &'static str,
    content: SectionContentCtx,
  ) -> Self {
    let anchor = render_context.toc.add_entry(1, title.to_string());

    Self {
      header: SectionHeaderCtx {
        title: title.to_string(),
        anchor: AnchorCtx { id: anchor },
        href: None,
        doc: None,
      },
      content,
    }
  }
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
  pub const TEMPLATE: &'static str = "tag";

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
  pub const TEMPLATE: &'static str = "doc_entry";

  #[allow(clippy::too_many_arguments)]
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

pub fn qualify_drilldown_name(
  parent_name: &str,
  drilldown_name: &str,
  is_static: bool,
) -> String {
  format!(
    "{parent_name}{}.{drilldown_name}",
    if is_static { "" } else { ".prototype" },
  )
}

#[derive(Debug, Serialize)]
pub struct TopSymbolCtx {
  pub kind: IndexSet<DocNodeKindCtx>,
  pub name: String,
  pub href: String,
}

#[derive(Debug, Serialize)]
pub struct TopSymbolsCtx {
  pub symbols: Vec<TopSymbolCtx>,
  pub total_symbols: usize,
  pub all_symbols_href: String,
}

impl TopSymbolsCtx {
  pub fn new(ctx: &RenderContext) -> Option<Self> {
    let partitions = ctx
      .ctx
      .doc_nodes
      .values()
      .flat_map(|nodes| super::partition::partition_nodes_by_name(nodes, true))
      .filter(|(_name, node)| !node[0].is_internal())
      .collect::<Vec<_>>();

    if partitions.is_empty() {
      return None;
    }

    let total_symbols = partitions.len();

    let symbols = partitions
      .into_iter()
      .take(5)
      .map(|(name, nodes)| TopSymbolCtx {
        kind: nodes
          .iter()
          .map(|node| node.kind_with_drilldown.into())
          .collect(),
        href: ctx.ctx.href_resolver.resolve_path(
          ctx.get_current_resolve(),
          UrlResolveKind::Symbol {
            file: &nodes[0].origin,
            symbol: &name,
          },
        ),
        name,
      })
      .collect();

    Some(Self {
      symbols,
      total_symbols,
      all_symbols_href: ctx
        .ctx
        .href_resolver
        .resolve_path(ctx.get_current_resolve(), UrlResolveKind::AllSymbols),
    })
  }
}

#[derive(Debug, Serialize)]
pub struct ToCCtx {
  pub usages: Option<UsagesCtx>,
  pub top_symbols: Option<TopSymbolsCtx>,
  pub document_navigation: Option<String>,
}

impl ToCCtx {
  pub const TEMPLATE: &'static str = "toc";

  pub fn new(
    ctx: RenderContext,
    include_top_symbols: bool,
    usage_doc_nodes: &[DocNodeWithContext],
  ) -> Self {
    Self {
      usages: if ctx.get_current_resolve() == UrlResolveKind::Root
        && ctx.ctx.main_entrypoint.is_none()
      {
        None
      } else {
        UsagesCtx::new(&ctx, usage_doc_nodes)
      },
      top_symbols: if include_top_symbols {
        TopSymbolsCtx::new(&ctx)
      } else {
        None
      },
      document_navigation: ctx.toc.render(),
    }
  }
}

pub fn slugify(name: &str) -> String {
  static REJECTED_CHARS: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"[^\p{L}\p{M}\p{N}\p{Pc} -]").unwrap());

  REJECTED_CHARS
    .replace_all(&name.to_lowercase(), "")
    .replace(' ', "-")
}

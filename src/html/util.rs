use crate::diff::JsDocDiff;
use crate::html::DiffStatus;
use crate::html::DocNodeKind;
use crate::html::DocNodeWithContext;
use crate::html::FileMode;
use crate::html::GenerateCtx;
use crate::html::RenderContext;
use crate::html::ShortPath;
use crate::html::render_context::ToCEntry;
use crate::html::usage::UsagesCtx;
use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;
use crate::node::DocNodeDef;
use deno_ast::swc::ast::Accessibility;
use deno_ast::swc::atoms::once_cell::sync::Lazy;
use indexmap::IndexSet;
use regex::Regex;
use serde::Deserialize;
use serde::Serialize;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Display;
use std::fmt::Formatter;
use std::rc::Rc;

lazy_static! {
  static ref TARGET_RE: Regex = Regex::new(r"\s*\* ?|\.").unwrap();
}

pub enum IdKind {
  Constructor,
  Property,
  Method,
  Function,
  Variable,
  Class,
  Enum,
  Interface,
  TypeAlias,
  Namespace,
  Accessor,
  Parameter,
  Return,
  TypeParam,
  Throws,
  IndexSignature,
  CallSignature,
  Example,
}

impl IdKind {
  fn as_str(&self) -> &'static str {
    match self {
      IdKind::Constructor => "constructor",
      IdKind::Property => "property",
      IdKind::Method => "method",
      IdKind::Function => "function",
      IdKind::Variable => "variable",
      IdKind::Class => "class",
      IdKind::Enum => "enum",
      IdKind::Interface => "interface",
      IdKind::TypeAlias => "typeAlias",
      IdKind::Namespace => "namespace",
      IdKind::Accessor => "accessor",
      IdKind::Parameter => "parameter",
      IdKind::Return => "return",
      IdKind::TypeParam => "type_param",
      IdKind::Throws => "throws",
      IdKind::IndexSignature => "index_signature",
      IdKind::CallSignature => "call_signature",
      IdKind::Example => "example",
    }
  }
}

pub struct IdBuilder<'a> {
  ctx: &'a RenderContext<'a>,
  components: Vec<Cow<'a, str>>,
}

impl<'a> IdBuilder<'a> {
  pub fn new(ctx: &'a RenderContext<'a>) -> Self {
    let mut builder = Self {
      ctx,
      components: Vec::new(),
    };

    if let Some(prefix) = &ctx.ctx.id_prefix {
      builder.components.push(Cow::Borrowed(prefix));
    }

    builder
  }

  pub fn new_with_parent(ctx: &'a RenderContext<'a>, parent: &Id) -> Self {
    let mut builder = Self::new(ctx);

    let component = parent.as_ref();
    if !component.is_empty() {
      builder
        .components
        .push(Cow::Owned(sanitize_id_part(component)));
    }

    builder
  }

  pub fn kind(mut self, kind: IdKind) -> Self {
    self.components.push(Cow::Borrowed(kind.as_str()));
    self
  }

  pub fn name(mut self, name: &str) -> Self {
    if !name.is_empty() {
      self.components.push(Cow::Owned(sanitize_id_part(name)));
    }
    self
  }

  pub fn index(mut self, index: usize) -> Self {
    self.components.push(Cow::Owned(index.to_string()));
    self
  }

  pub fn build(self) -> Id {
    self.ctx.toc.anchorize(&self.components.join("_"))
  }

  /// Build an ID without registering it in the anchorizer.
  /// Use this for IDs that reference anchors on other pages (e.g. href targets).
  pub fn build_unregistered(self) -> Id {
    self.ctx.toc.sanitize(&self.components.join("_"))
  }
}

#[derive(
  Debug,
  Clone,
  Serialize,
  Deserialize,
  Ord,
  PartialOrd,
  Eq,
  PartialEq,
  Hash,
  Default,
)]
pub struct Id(String);

impl Id {
  pub(crate) fn as_str(&self) -> &str {
    self.0.as_str()
  }

  pub(crate) fn from_raw(s: String) -> Self {
    Id(s)
  }

  pub fn empty() -> Self {
    Id(String::new())
  }
}

impl Display for Id {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.0)
  }
}

impl AsRef<str> for Id {
  fn as_ref(&self) -> &str {
    self.0.as_str()
  }
}

fn sanitize_id_part(part: &str) -> String {
  html_escape::encode_quoted_attribute(&TARGET_RE.replace_all(part, "_"))
    .into_owned()
}

/// A container to hold a list of symbols with their namespaces:
///
/// ["setTimeout"]
/// ["Deno", "read"]
/// ["Deno", "errors"]
/// ["Deno", "errors", "HttpError"]
#[derive(Clone, Debug)]
pub(crate) struct NamespacedSymbols(
  Rc<HashMap<Vec<String>, Option<Rc<ShortPath>>>>,
);

impl NamespacedSymbols {
  pub(crate) fn new(
    ctx: &GenerateCtx,
    doc_nodes: &[DocNodeWithContext],
  ) -> Self {
    let symbols = compute_namespaced_symbols(
      ctx,
      Box::new(doc_nodes.iter().map(Cow::Borrowed)),
    );
    Self(Rc::new(symbols))
  }

  pub(crate) fn get(&self, path: &[String]) -> Option<&Option<Rc<ShortPath>>> {
    self.0.get(path)
  }
}

pub fn compute_namespaced_symbols<'a>(
  ctx: &'a GenerateCtx,
  doc_nodes: Box<dyn Iterator<Item = Cow<'a, DocNodeWithContext>> + 'a>,
) -> HashMap<Vec<String>, Option<Rc<ShortPath>>> {
  let mut namespaced_symbols =
    HashMap::<Vec<String>, Option<Rc<ShortPath>>>::new();

  for doc_node in doc_nodes {
    if matches!(
      doc_node.def,
      DocNodeDef::ModuleDoc | DocNodeDef::Import { .. }
    ) {
      continue;
    }

    // TODO: handle export aliasing

    let name_path: Rc<[String]> = doc_node.sub_qualifier().into();

    match &doc_node.def {
      DocNodeDef::Class { class_def } => {
        namespaced_symbols.extend(class_def.methods.iter().map(|method| {
          let mut method_path = doc_node.ns_qualifiers.to_vec();
          method_path.extend(
            qualify_drilldown_name(
              doc_node.get_name(),
              &method.name,
              method.is_static,
            )
            .split('.')
            .map(|part| part.to_string()),
          );
          (method_path, Some(doc_node.origin.clone()))
        }));

        namespaced_symbols.extend(class_def.properties.iter().map(
          |property| {
            let mut method_path = doc_node.ns_qualifiers.to_vec();
            method_path.extend(
              qualify_drilldown_name(
                doc_node.get_name(),
                &property.name,
                property.is_static,
              )
              .split('.')
              .map(|part| part.to_string()),
            );
            (method_path, Some(doc_node.origin.clone()))
          },
        ));
      }
      DocNodeDef::Interface { interface_def } => {
        namespaced_symbols.extend(interface_def.methods.iter().map(|method| {
          let mut method_path = doc_node.ns_qualifiers.to_vec();
          method_path.extend(
            qualify_drilldown_name(doc_node.get_name(), &method.name, true)
              .split('.')
              .map(|part| part.to_string()),
          );
          (method_path, Some(doc_node.origin.clone()))
        }));

        namespaced_symbols.extend(interface_def.properties.iter().map(
          |property| {
            let mut method_path = doc_node.ns_qualifiers.to_vec();
            method_path.extend(
              qualify_drilldown_name(doc_node.get_name(), &property.name, true)
                .split('.')
                .map(|part| part.to_string()),
            );
            (method_path, Some(doc_node.origin.clone()))
          },
        ));
      }
      DocNodeDef::TypeAlias { type_alias_def } => {
        if let Some(type_literal) = type_alias_def.ts_type.type_literal.as_ref()
        {
          namespaced_symbols.extend(type_literal.methods.iter().map(
            |method| {
              let mut method_path = doc_node.ns_qualifiers.to_vec();
              method_path.extend(
                qualify_drilldown_name(doc_node.get_name(), &method.name, true)
                  .split('.')
                  .map(|part| part.to_string()),
              );
              (method_path, Some(doc_node.origin.clone()))
            },
          ));

          namespaced_symbols.extend(type_literal.properties.iter().map(
            |property| {
              let mut method_path = doc_node.ns_qualifiers.to_vec();
              method_path.extend(
                qualify_drilldown_name(
                  doc_node.get_name(),
                  &property.name,
                  true,
                )
                .split('.')
                .map(|part| part.to_string()),
              );
              (method_path, Some(doc_node.origin.clone()))
            },
          ));
        }
      }
      DocNodeDef::Variable { variable_def } => {
        if let Some(type_literal) = variable_def
          .ts_type
          .as_ref()
          .and_then(|ts_type| ts_type.type_literal.as_ref())
        {
          namespaced_symbols.extend(type_literal.methods.iter().map(
            |method| {
              let mut method_path = doc_node.ns_qualifiers.to_vec();
              method_path.extend(
                qualify_drilldown_name(doc_node.get_name(), &method.name, true)
                  .split('.')
                  .map(|part| part.to_string()),
              );
              (method_path, Some(doc_node.origin.clone()))
            },
          ));

          namespaced_symbols.extend(type_literal.properties.iter().map(
            |property| {
              let mut method_path = doc_node.ns_qualifiers.to_vec();
              method_path.extend(
                qualify_drilldown_name(
                  doc_node.get_name(),
                  &property.name,
                  true,
                )
                .split('.')
                .map(|part| part.to_string()),
              );
              (method_path, Some(doc_node.origin.clone()))
            },
          ));
        }
      }
      _ => {}
    }

    namespaced_symbols
      .insert(name_path.to_vec(), Some(doc_node.origin.clone()));

    if matches!(doc_node.def, DocNodeDef::Namespace { .. }) {
      let children = doc_node
        .namespace_children
        .as_ref()
        .unwrap()
        .iter()
        .flat_map(|element| {
          if let Some(reference_def) = element.reference_def() {
            Box::new(
              ctx.resolve_reference(Some(&doc_node), &reference_def.target),
            ) as Box<dyn Iterator<Item = Cow<DocNodeWithContext>>>
          } else {
            Box::new(std::iter::once(Cow::Borrowed(element))) as _
          }
        });

      namespaced_symbols
        .extend(compute_namespaced_symbols(ctx, Box::new(children)))
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
#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum UrlResolveKind<'a> {
  Root,
  AllSymbols,
  Category {
    category: &'a str,
  },
  File {
    file: &'a ShortPath,
  },
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
      UrlResolveKind::Category { .. } => None,
      UrlResolveKind::File { file } => Some(file),
      UrlResolveKind::Symbol { file, .. } => Some(file),
    }
  }
}

pub fn href_path_resolve(
  current: UrlResolveKind,
  target: UrlResolveKind,
) -> String {
  let backs = match current {
    UrlResolveKind::File { file } => "../".repeat(if file.is_main {
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
    UrlResolveKind::Category { .. } => String::from("./"),
  };

  match target {
    UrlResolveKind::Root => backs,
    UrlResolveKind::File { file: target_file } if target_file.is_main => backs,
    UrlResolveKind::AllSymbols => format!("{backs}./all_symbols.html"),
    UrlResolveKind::Symbol {
      file: target_file,
      symbol: target_symbol,
      ..
    } => {
      format!("{backs}./{}/~/{target_symbol}.html", target_file.path)
    }
    UrlResolveKind::File { file: target_file } => {
      format!("{backs}./{}/index.html", target_file.path)
    }
    UrlResolveKind::Category { category } => {
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

  /// Resolve the URL used in source code link buttons.
  fn resolve_source(&self, location: &crate::Location) -> Option<String>;

  /// Resolve external JSDoc module links.
  /// Returns a tuple with link and title.
  fn resolve_external_jsdoc_module(
    &self,
    module: &str,
    symbol: Option<&str>,
  ) -> Option<(String, String)>;
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct BreadcrumbCtx {
  pub name: String,
  pub href: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct BreadcrumbsCtx {
  pub root: BreadcrumbCtx,
  pub current_entrypoint: Option<BreadcrumbCtx>,
  pub entrypoints: Vec<BreadcrumbCtx>,
  pub symbol: Vec<BreadcrumbCtx>,
}

impl BreadcrumbsCtx {
  pub const TEMPLATE: &'static str = "breadcrumbs";

  pub fn to_strings(&self) -> Vec<Cow<'_, str>> {
    let mut title_parts = vec![Cow::Borrowed(self.root.name.as_str())];

    if let Some(entrypoint) = &self.current_entrypoint {
      title_parts.push(Cow::Borrowed(entrypoint.name.as_str()));
    }

    if !self.symbol.is_empty() {
      title_parts.push(Cow::Owned(
        self
          .symbol
          .iter()
          .map(|crumb| crumb.name.as_str())
          .collect::<Vec<_>>()
          .join("."),
      ));
    }

    title_parts
  }
}

#[derive(Debug, Serialize, Deserialize, Clone, Eq, PartialEq, Hash)]
pub struct DocNodeKindCtx {
  pub kind: Cow<'static, str>,
  pub char: char,
  pub title: Cow<'static, str>,
  pub title_lowercase: Cow<'static, str>,
  pub title_plural: Cow<'static, str>,
}

impl From<DocNodeKind> for DocNodeKindCtx {
  fn from(kind: DocNodeKind) -> Self {
    let (char, kind, title, title_lowercase, title_plural) = match kind {
      DocNodeKind::Property => {
        ('p', "Property", "Property", "property", "Properties")
      }
      DocNodeKind::Method(_) => ('m', "Method", "Method", "method", "Methods"),
      DocNodeKind::Function => {
        ('f', "Function", "Function", "function", "Functions")
      }
      DocNodeKind::Variable => {
        ('v', "Variable", "Variable", "variable", "Variables")
      }
      DocNodeKind::Class => ('c', "Class", "Class", "class", "Classes"),
      DocNodeKind::Enum => ('E', "Enum", "Enum", "enum", "Enums"),
      DocNodeKind::Interface => {
        ('I', "Interface", "Interface", "interface", "Interfaces")
      }
      DocNodeKind::TypeAlias => {
        ('T', "TypeAlias", "Type Alias", "type alias", "Type Aliases")
      }
      DocNodeKind::Namespace => {
        ('N', "Namespace", "Namespace", "namespace", "Namespaces")
      }
      DocNodeKind::ModuleDoc | DocNodeKind::Import | DocNodeKind::Reference => {
        unreachable!()
      }
    };

    Self {
      kind: kind.into(),
      char,
      title: title.into(),
      title_lowercase: title_lowercase.into(),
      title_plural: title_plural.into(),
    }
  }
}

#[derive(
  Debug, Serialize, Deserialize, Clone, Ord, PartialOrd, Eq, PartialEq, Hash,
)]
pub struct AnchorCtx {
  pub id: Id,
}

impl AnchorCtx {
  pub const TEMPLATE: &'static str = "anchor";

  pub fn new(id: Id) -> Self {
    Self { id }
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "snake_case", tag = "kind", content = "content")]
pub enum SectionContentCtx {
  DocEntry(Vec<DocEntryCtx>),
  Example(Vec<super::jsdoc::ExampleCtx>),
  IndexSignature(Vec<super::symbols::class::IndexSignatureCtx>),
  NamespaceSection(Vec<super::namespace::NamespaceNodeCtx>),
  See(Vec<String>),
  Empty,
}

#[derive(
  Debug, Serialize, Deserialize, Clone, Ord, PartialOrd, Eq, PartialEq, Hash,
)]
pub struct SectionHeaderCtx {
  pub title: String,
  pub anchor: AnchorCtx,
  pub href: Option<String>,
  pub doc: Option<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct SectionCtx {
  pub header: Option<SectionHeaderCtx>,
  pub content: SectionContentCtx,
}

impl SectionCtx {
  pub const TEMPLATE: &'static str = "section";

  pub fn new(
    render_context: &RenderContext,
    title: &str,
    content: SectionContentCtx,
  ) -> Self {
    let header = if !title.is_empty() {
      let id = render_context.toc.anchorize(title);
      render_context.toc.add_entry(1, title, &id);

      Some(SectionHeaderCtx {
        title: title.to_string(),
        anchor: AnchorCtx::new(id),
        href: None,
        doc: None,
      })
    } else {
      None
    };

    match &content {
      SectionContentCtx::DocEntry(entries) => {
        for entry in entries {
          let Some(name) = &entry.name else {
            continue;
          };

          render_context.toc.add_entry(2, name, &entry.anchor.id);
        }
      }
      SectionContentCtx::Example(examples) => {
        for example in examples {
          render_context.toc.add_entry(
            2,
            &super::jsdoc::strip(render_context, &example.title),
            &example.anchor.id,
          );
        }
      }
      SectionContentCtx::IndexSignature(_) => {}
      SectionContentCtx::NamespaceSection(nodes) => {
        for node in nodes {
          render_context.toc.add_entry(2, &node.name, &node.anchor.id);
        }
      }
      SectionContentCtx::See(_) => {}
      SectionContentCtx::Empty => {}
    }

    Self { header, content }
  }
}

#[derive(Debug, Serialize, Deserialize, Clone, Eq, PartialEq, Hash)]
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
  Unstable,
  Permissions(Box<[Box<str>]>),
  Other(Box<str>),
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

  pub fn from_js_doc(js_doc: &JsDoc) -> IndexSet<Tag> {
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

#[derive(Debug, Serialize, Deserialize, Clone, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum TagDiffKind {
  Added,
  Removed,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct TagCtx {
  #[serde(flatten)]
  pub tag: Tag,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub diff: Option<TagDiffKind>,
}

pub fn compute_tag_ctx(
  current: IndexSet<Tag>,
  old: Option<IndexSet<Tag>>,
) -> Vec<TagCtx> {
  match old {
    None => current
      .into_iter()
      .map(|tag| TagCtx { tag, diff: None })
      .collect(),
    Some(old) => {
      let mut result = Vec::new();
      for tag in &current {
        let diff = if old.contains(tag) {
          None
        } else {
          Some(TagDiffKind::Added)
        };
        result.push(TagCtx {
          tag: tag.clone(),
          diff,
        });
      }
      for tag in &old {
        if !current.contains(tag) {
          result.push(TagCtx {
            tag: tag.clone(),
            diff: Some(TagDiffKind::Removed),
          });
        }
      }
      result
    }
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct DocEntryCtx {
  pub name_prefix: Option<Cow<'static, str>>,
  name: Option<String>,
  name_href: Option<String>,
  content: String,
  pub anchor: AnchorCtx,
  pub tags: Vec<TagCtx>,
  js_doc: Option<String>,
  source_href: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub diff_status: Option<DiffStatus>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub old_content: Option<String>,
}

impl DocEntryCtx {
  pub const TEMPLATE: &'static str = "doc_entry";

  #[allow(clippy::too_many_arguments)]
  pub fn new(
    ctx: &RenderContext,
    id: Id,
    name: Option<String>,
    name_href: Option<String>,
    content: &str,
    tags: IndexSet<Tag>,
    jsdoc: Option<&str>,
    location: &crate::Location,
    diff_status: Option<DiffStatus>,
    old_content: Option<String>,
    old_tags: Option<IndexSet<Tag>>,
    js_doc_diff: Option<&JsDocDiff>,
  ) -> Self {
    let maybe_jsdoc = if let Some(doc_change) =
      js_doc_diff.and_then(|d| d.doc_change.as_ref())
    {
      let old_doc = doc_change.old.as_deref().unwrap_or_default();
      let new_doc = jsdoc.unwrap_or_default();
      crate::html::jsdoc::render_docs_with_diff(ctx, old_doc, new_doc)
    } else {
      jsdoc.map(|doc| crate::html::jsdoc::render_markdown(ctx, doc, true))
    };
    let source_href = ctx.ctx.href_resolver.resolve_source(location);

    DocEntryCtx {
      name_prefix: None,
      name,
      name_href,
      content: content.to_string(),
      anchor: AnchorCtx::new(id),
      tags: compute_tag_ctx(tags, old_tags),
      js_doc: maybe_jsdoc,
      source_href,
      diff_status,
      old_content,
    }
  }

  #[allow(clippy::too_many_arguments)]
  pub fn removed(
    ctx: &RenderContext,
    id: Id,
    name: Option<String>,
    name_href: Option<String>,
    content: &str,
    tags: IndexSet<Tag>,
    jsdoc: Option<&str>,
    location: &crate::Location,
  ) -> Self {
    Self::new(
      ctx,
      id,
      name,
      name_href,
      content,
      tags,
      jsdoc,
      location,
      Some(DiffStatus::Removed),
      None,
      None,
      None,
    )
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

#[derive(Debug, Serialize, Deserialize)]
pub struct TopSymbolCtx {
  pub kind: IndexSet<DocNodeKindCtx>,
  pub name: String,
  pub href: String,
}

#[derive(Debug, Serialize, Deserialize)]
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
      .flat_map(|nodes| {
        super::partition::partition_nodes_by_name(
          ctx.ctx,
          nodes.iter().map(Cow::Borrowed),
          true,
        )
      })
      .filter(|(_name, node)| !node[0].is_internal(ctx.ctx))
      .collect::<Vec<_>>();

    if partitions.is_empty() {
      return None;
    }

    let total_symbols = partitions.len();

    let symbols = partitions
      .into_iter()
      .take(5)
      .map(|(name, nodes)| TopSymbolCtx {
        kind: nodes.iter().map(|node| node.kind.into()).collect(),
        href: ctx.ctx.resolve_path(
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
        .resolve_path(ctx.get_current_resolve(), UrlResolveKind::AllSymbols),
    })
  }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ToCCtx {
  pub usages: Option<UsagesCtx>,
  pub top_symbols: Option<TopSymbolsCtx>,
  pub document_navigation_str: Option<String>,
  pub document_navigation: Vec<ToCEntry>,
}

impl ToCCtx {
  pub const TEMPLATE: &'static str = "toc";

  pub fn new(
    ctx: RenderContext,
    include_top_symbols: bool,
    usage_doc_nodes: Option<&[DocNodeWithContext]>,
  ) -> Self {
    if ctx.get_current_resolve() == UrlResolveKind::Root
      && matches!(ctx.ctx.file_mode, FileMode::SingleDts | FileMode::Dts)
    {
      return Self {
        usages: None,
        top_symbols: None,
        document_navigation_str: None,
        document_navigation: vec![],
      };
    }

    Self {
      usages: if ctx.get_current_resolve() == UrlResolveKind::Root
        && ctx.ctx.main_entrypoint.is_none()
      {
        None
      } else {
        usage_doc_nodes
          .and_then(|usage_doc_nodes| UsagesCtx::new(&ctx, usage_doc_nodes))
      },
      top_symbols: if include_top_symbols {
        TopSymbolsCtx::new(&ctx)
      } else {
        None
      },
      document_navigation_str: ctx.toc.render(),
      document_navigation: std::sync::Arc::into_inner(ctx.toc.toc)
        .unwrap()
        .into_inner()
        .unwrap(),
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

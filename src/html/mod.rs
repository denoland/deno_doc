use crate::node::DocNodeDef;
use crate::DocNode;
use deno_ast::swc::ast::MethodKind;
use deno_ast::ModuleSpecifier;
use handlebars::handlebars_helper;
use handlebars::Handlebars;
use indexmap::IndexMap;
use serde::Deserialize;
use serde::Serialize;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

pub mod jsdoc;
pub mod pages;
mod parameters;
pub mod partition;
mod render_context;
mod search;
mod symbols;
mod types;
mod usage;
pub mod util;

#[cfg(feature = "comrak")]
pub mod comrak;

use crate::html::pages::SymbolPage;
use crate::js_doc::JsDocTag;
pub use pages::generate_symbol_pages_for_module;
pub use render_context::RenderContext;
pub use search::generate_search_index;
pub use symbols::namespace;
pub use symbols::SymbolContentCtx;
pub use symbols::SymbolGroupCtx;
pub use usage::UsageComposer;
pub use usage::UsageComposerEntry;
pub use usage::UsageToMd;
pub use util::compute_namespaced_symbols;
pub use util::href_path_resolve;
pub use util::qualify_drilldown_name;
pub use util::DocNodeKindCtx;
pub use util::HrefResolver;
pub use util::NamespacedGlobalSymbols;
pub use util::SectionHeaderCtx;
pub use util::ToCCtx;
pub use util::TopSymbolCtx;
pub use util::TopSymbolsCtx;
pub use util::UrlResolveKind;

pub const STYLESHEET: &str = include_str!("./templates/styles.gen.css");
pub const STYLESHEET_FILENAME: &str = "styles.css";

pub const PAGE_STYLESHEET: &str =
  include_str!("./templates/pages/page.gen.css");
pub const PAGE_STYLESHEET_FILENAME: &str = "page.css";

pub const RESET_STYLESHEET: &str =
  include_str!("./templates/pages/reset.gen.css");
pub const RESET_STYLESHEET_FILENAME: &str = "reset.css";

const SEARCH_INDEX_FILENAME: &str = "search_index.js";

pub const SCRIPT_JS: &str = include_str!("./templates/script.js");
pub const SCRIPT_FILENAME: &str = "script.js";

const FUSE_JS: &str = include_str!("./templates/pages/fuse.js");
const FUSE_FILENAME: &str = "fuse.js";

const SEARCH_JS: &str = include_str!("./templates/pages/search.js");
const SEARCH_FILENAME: &str = "search.js";

fn setup_hbs() -> Result<Handlebars<'static>, handlebars::TemplateError> {
  let mut reg = Handlebars::new();
  reg.register_escape_fn(|str| html_escape::encode_safe(str).into_owned());
  reg.set_strict_mode(true);

  #[cfg(debug_assertions)]
  reg.set_dev_mode(true);

  handlebars_helper!(concat: |a: str, b: str| format!("{a}{b}"));
  reg.register_helper("concat", Box::new(concat));

  handlebars_helper!(print: |a: Json| println!("{a:#?}"));
  reg.register_helper("print", Box::new(print));

  reg.register_template_string(
    ToCCtx::TEMPLATE,
    include_str!("./templates/toc.hbs"),
  )?;
  reg.register_template_string(
    util::DocEntryCtx::TEMPLATE,
    include_str!("./templates/doc_entry.hbs"),
  )?;
  reg.register_template_string(
    util::SectionCtx::TEMPLATE,
    include_str!("./templates/section.hbs"),
  )?;
  reg.register_template_string(
    "doc_node_kind_icon",
    include_str!("./templates/doc_node_kind_icon.hbs"),
  )?;
  reg.register_template_string(
    "namespace_section",
    include_str!("./templates/namespace_section.hbs"),
  )?;
  reg.register_template_string(
    symbols::DocBlockSubtitleCtx::TEMPLATE_CLASS,
    include_str!("./templates/doc_block_subtitle_class.hbs"),
  )?;
  reg.register_template_string(
    symbols::DocBlockSubtitleCtx::TEMPLATE_INTERFACE,
    include_str!("./templates/doc_block_subtitle_interface.hbs"),
  )?;
  reg.register_template_string(
    util::AnchorCtx::TEMPLATE,
    include_str!("./templates/anchor.hbs"),
  )?;
  reg.register_template_string(
    SymbolGroupCtx::TEMPLATE,
    include_str!("./templates/symbol_group.hbs"),
  )?;
  reg.register_template_string(
    SymbolContentCtx::TEMPLATE,
    include_str!("./templates/symbol_content.hbs"),
  )?;
  reg.register_template_string(
    jsdoc::ExampleCtx::TEMPLATE,
    include_str!("./templates/example.hbs"),
  )?;
  reg.register_template_string(
    symbols::function::FunctionCtx::TEMPLATE,
    include_str!("./templates/function.hbs"),
  )?;
  reg.register_template_string(
    jsdoc::ModuleDocCtx::TEMPLATE,
    include_str!("./templates/module_doc.hbs"),
  )?;
  reg.register_template_string(
    util::BreadcrumbsCtx::TEMPLATE,
    include_str!("./templates/breadcrumbs.hbs"),
  )?;
  reg.register_template_string(
    usage::UsagesCtx::TEMPLATE,
    include_str!("./templates/usages.hbs"),
  )?;
  reg.register_template_string(
    "usages_large",
    include_str!("./templates/usages_large.hbs"),
  )?;
  reg.register_template_string(
    util::Tag::TEMPLATE,
    include_str!("./templates/tag.hbs"),
  )?;
  reg.register_template_string(
    "source_button",
    include_str!("./templates/source_button.hbs"),
  )?;
  reg.register_template_string(
    "deprecated",
    include_str!("./templates/deprecated.hbs"),
  )?;
  reg.register_template_string(
    "index_signature",
    include_str!("./templates/index_signature.hbs"),
  )?;
  reg.register_template_string(
    pages::CategoriesPanelCtx::TEMPLATE,
    include_str!("./templates/category_panel.hbs"),
  )?;
  reg.register_template_string("see", include_str!("./templates/see.hbs"))?;

  // pages
  reg.register_template_string(
    pages::HtmlHeadCtx::TEMPLATE,
    include_str!("./templates/pages/html_head.hbs"),
  )?;
  reg.register_template_string(
    pages::AllSymbolsCtx::TEMPLATE,
    include_str!("./templates/pages/all_symbols.hbs"),
  )?;
  reg.register_template_string(
    pages::SymbolPageCtx::TEMPLATE,
    include_str!("./templates/pages/symbol.hbs"),
  )?;
  reg.register_template_string(
    pages::IndexCtx::TEMPLATE,
    include_str!("./templates/pages/index.hbs"),
  )?;
  reg.register_template_string(
    "pages/top_nav",
    include_str!("./templates/pages/top_nav.hbs"),
  )?;
  reg.register_template_string(
    "pages/search_results",
    include_str!("./templates/pages/search_results.hbs"),
  )?;
  reg.register_template_string(
    "pages/redirect",
    include_str!("./templates/pages/redirect.hbs"),
  )?;

  // icons
  reg.register_template_string(
    "icons/arrow",
    include_str!("./templates/icons/arrow.svg"),
  )?;
  reg.register_template_string(
    "icons/copy",
    include_str!("./templates/icons/copy.svg"),
  )?;
  reg.register_template_string(
    "icons/check",
    include_str!("./templates/icons/check.svg"),
  )?;
  reg.register_template_string(
    "icons/link",
    include_str!("./templates/icons/link.svg"),
  )?;
  reg.register_template_string(
    "icons/source",
    include_str!("./templates/icons/source.svg"),
  )?;
  reg.register_template_string(
    "icons/menu",
    include_str!("./templates/icons/menu.svg"),
  )?;

  Ok(reg)
}

lazy_static! {
  pub static ref HANDLEBARS: Handlebars<'static> = setup_hbs().unwrap();
}

pub type HeadInject = Rc<dyn Fn(&str) -> String>;

#[derive(Clone)]
pub struct GenerateOptions {
  /// The name that is shown is the top-left corner, eg. "deno_std".
  pub package_name: Option<String>,
  /// The main entrypoint.
  /// If only a single file is specified during generation, this will always
  /// default to that file.
  pub main_entrypoint: Option<ModuleSpecifier>,
  pub href_resolver: Rc<dyn HrefResolver>,
  pub usage_composer: Rc<dyn UsageComposer>,
  pub rewrite_map: Option<IndexMap<ModuleSpecifier, String>>,
  pub category_docs: Option<IndexMap<String, Option<String>>>,
  pub disable_search: bool,
  pub symbol_redirect_map: Option<IndexMap<String, IndexMap<String, String>>>,
  pub default_symbol_map: Option<IndexMap<String, String>>,
  pub markdown_renderer: jsdoc::MarkdownRenderer,
  pub markdown_stripper: jsdoc::MarkdownStripper,
  pub head_inject: Option<HeadInject>,
}

#[non_exhaustive]
pub struct GenerateCtx {
  pub package_name: Option<String>,
  pub common_ancestor: Option<PathBuf>,
  pub doc_nodes: IndexMap<Rc<ShortPath>, Vec<DocNodeWithContext>>,
  pub href_resolver: Rc<dyn HrefResolver>,
  pub usage_composer: Rc<dyn UsageComposer>,
  pub rewrite_map: Option<IndexMap<ModuleSpecifier, String>>,
  pub main_entrypoint: Option<Rc<ShortPath>>,
  pub file_mode: FileMode,
  pub category_docs: Option<IndexMap<String, Option<String>>>,
  pub disable_search: bool,
  pub symbol_redirect_map: Option<IndexMap<String, IndexMap<String, String>>>,
  pub default_symbol_map: Option<IndexMap<String, String>>,
  pub markdown_renderer: jsdoc::MarkdownRenderer,
  pub markdown_stripper: jsdoc::MarkdownStripper,
  pub head_inject: Option<HeadInject>,
}

impl GenerateCtx {
  pub fn new(
    options: GenerateOptions,
    common_ancestor: Option<PathBuf>,
    file_mode: FileMode,
    doc_nodes_by_url: IndexMap<ModuleSpecifier, Vec<DocNode>>,
  ) -> Self {
    let mut main_entrypoint = None;

    let doc_nodes = doc_nodes_by_url
      .into_iter()
      .map(|(specifier, nodes)| {
        let short_path = Rc::new(ShortPath::new(
          specifier,
          options.main_entrypoint.as_ref(),
          options.rewrite_map.as_ref(),
          common_ancestor.as_ref(),
        ));

        if short_path.is_main {
          main_entrypoint = Some(short_path.clone());
        }

        let nodes = nodes
          .into_iter()
          .map(|mut node| {
            if &*node.name == "default" {
              if let Some(default_rename) =
                options.default_symbol_map.as_ref().and_then(
                  |default_symbol_map| default_symbol_map.get(&short_path.path),
                )
              {
                node.name = default_rename.as_str().into();
              }
            }

            let node = if node
              .variable_def()
              .as_ref()
              .and_then(|def| def.ts_type.as_ref())
              .and_then(|ts_type| ts_type.kind.as_ref())
              .is_some_and(|kind| {
                kind == &crate::ts_type::TsTypeDefKind::FnOrConstructor
              }) {
              let DocNodeDef::Variable { variable_def } = node.def else {
                unreachable!()
              };
              let fn_or_constructor =
                variable_def.ts_type.unwrap().fn_or_constructor.unwrap();

              let mut new_node = DocNode::function(
                node.name,
                false,
                node.location,
                node.declaration_kind,
                node.js_doc,
                crate::function::FunctionDef {
                  def_name: None,
                  params: fn_or_constructor.params,
                  return_type: Some(fn_or_constructor.ts_type),
                  has_body: false,
                  is_async: false,
                  is_generator: false,
                  type_params: fn_or_constructor.type_params,
                  decorators: Box::new([]),
                },
              );
              new_node.is_default = node.is_default;
              new_node
            } else {
              node
            };

            DocNodeWithContext {
              origin: short_path.clone(),
              ns_qualifiers: Rc::new([]),
              kind_with_drilldown: DocNodeKindWithDrilldown::Other(node.kind()),
              inner: Rc::new(node),
              drilldown_name: None,
              parent: None,
            }
          })
          .collect::<Vec<_>>();

        (short_path, nodes)
      })
      .collect::<IndexMap<_, _>>();

    Self {
      package_name: options.package_name,
      common_ancestor,
      doc_nodes,
      href_resolver: options.href_resolver,
      usage_composer: options.usage_composer,
      rewrite_map: options.rewrite_map,
      main_entrypoint,
      file_mode,
      category_docs: options.category_docs,
      disable_search: options.disable_search,
      symbol_redirect_map: options.symbol_redirect_map,
      default_symbol_map: options.default_symbol_map,
      markdown_renderer: options.markdown_renderer,
      markdown_stripper: options.markdown_stripper,
      head_inject: options.head_inject,
    }
  }

  pub fn render<T: serde::Serialize>(
    &self,
    template: &str,
    data: &T,
  ) -> String {
    HANDLEBARS.render(template, data).unwrap()
  }

  pub fn resolve_path(
    &self,
    current: UrlResolveKind,
    target: UrlResolveKind,
  ) -> String {
    if let Some(symbol_redirect_map) = &self.symbol_redirect_map {
      if let UrlResolveKind::Symbol { file, symbol } = target {
        if let Some(path_map) = symbol_redirect_map.get(&file.path) {
          if let Some(href) = path_map.get(symbol) {
            return href.clone();
          }
        }
      }
    }

    self.href_resolver.resolve_path(current, target)
  }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ShortPath {
  pub path: String,
  pub specifier: ModuleSpecifier,
  pub is_main: bool,
}

impl ShortPath {
  pub fn new(
    specifier: ModuleSpecifier,
    main_entrypoint: Option<&ModuleSpecifier>,
    rewrite_map: Option<&IndexMap<ModuleSpecifier, String>>,
    common_ancestor: Option<&PathBuf>,
  ) -> Self {
    let is_main = main_entrypoint
      .is_some_and(|main_entrypoint| main_entrypoint == &specifier);

    if let Some(rewrite) =
      rewrite_map.and_then(|rewrite_map| rewrite_map.get(&specifier))
    {
      return ShortPath {
        path: rewrite
          .strip_prefix('.')
          .unwrap_or(rewrite)
          .strip_prefix('/')
          .unwrap_or(rewrite)
          .to_owned(),
        specifier,
        is_main,
      };
    }

    let Ok(url_file_path) = deno_path_util::url_to_file_path(&specifier) else {
      return ShortPath {
        path: specifier.to_string(),
        specifier,
        is_main,
      };
    };

    let Some(common_ancestor) = common_ancestor else {
      return ShortPath {
        path: url_file_path.to_string_lossy().to_string(),
        specifier,
        is_main,
      };
    };

    let stripped_path = url_file_path
      .strip_prefix(common_ancestor)
      .unwrap_or(&url_file_path);

    let path = stripped_path.to_string_lossy().to_string();

    ShortPath {
      path: if path.is_empty() {
        ".".to_string()
      } else {
        path
      },
      specifier,
      is_main,
    }
  }

  pub fn display_name(&self) -> &str {
    if self.is_main {
      "default"
    } else {
      self
        .path
        .strip_prefix('.')
        .unwrap_or(&self.path)
        .strip_prefix('/')
        .unwrap_or(&self.path)
    }
  }

  pub fn as_resolve_kind(&self) -> UrlResolveKind {
    if self.is_main {
      UrlResolveKind::Root
    } else {
      UrlResolveKind::File { file: self }
    }
  }
}

impl Ord for ShortPath {
  fn cmp(&self, other: &Self) -> Ordering {
    other
      .is_main
      .cmp(&self.is_main)
      .then_with(|| self.display_name().cmp(other.display_name()))
  }
}

impl PartialOrd for ShortPath {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Serialize, Deserialize)]
pub enum DocNodeKindWithDrilldown {
  Property,
  Method(MethodKind),
  Other(crate::DocNodeKind),
}

impl PartialOrd for DocNodeKindWithDrilldown {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for DocNodeKindWithDrilldown {
  fn cmp(&self, other: &Self) -> Ordering {
    match (self, other) {
      (
        DocNodeKindWithDrilldown::Other(other1),
        DocNodeKindWithDrilldown::Other(other2),
      ) => other1.cmp(other2),
      (
        DocNodeKindWithDrilldown::Property,
        DocNodeKindWithDrilldown::Other(_),
      ) => Ordering::Less,
      (
        DocNodeKindWithDrilldown::Property,
        DocNodeKindWithDrilldown::Method(_),
      ) => Ordering::Less,
      (
        DocNodeKindWithDrilldown::Property,
        DocNodeKindWithDrilldown::Property,
      ) => Ordering::Equal,
      (
        DocNodeKindWithDrilldown::Method(_),
        DocNodeKindWithDrilldown::Other(_),
      ) => Ordering::Less,
      (
        DocNodeKindWithDrilldown::Method(_),
        DocNodeKindWithDrilldown::Method(_),
      ) => Ordering::Equal,
      (
        DocNodeKindWithDrilldown::Method(_),
        DocNodeKindWithDrilldown::Property,
      ) => Ordering::Greater,
      (
        DocNodeKindWithDrilldown::Other(_),
        DocNodeKindWithDrilldown::Property,
      ) => Ordering::Greater,
      (
        DocNodeKindWithDrilldown::Other(_),
        DocNodeKindWithDrilldown::Method(_),
      ) => Ordering::Greater,
    }
  }
}

/// A wrapper around [`DocNode`] with additional fields to track information
/// about the inner [`DocNode`].
/// This is cheap to clone since all fields are [`Rc`]s.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DocNodeWithContext {
  pub origin: Rc<ShortPath>,
  pub ns_qualifiers: Rc<[String]>,
  pub kind_with_drilldown: DocNodeKindWithDrilldown,
  pub inner: Rc<DocNode>,
  pub drilldown_name: Option<Box<str>>,
  pub parent: Option<Box<DocNodeWithContext>>,
}

impl DocNodeWithContext {
  pub fn create_child(&self, doc_node: Rc<DocNode>) -> Self {
    DocNodeWithContext {
      origin: self.origin.clone(),
      ns_qualifiers: self.ns_qualifiers.clone(),
      kind_with_drilldown: DocNodeKindWithDrilldown::Other(doc_node.kind()),
      inner: doc_node,
      drilldown_name: None,
      parent: Some(Box::new(self.clone())),
    }
  }

  pub fn create_namespace_child(
    &self,
    doc_node: Rc<DocNode>,
    qualifiers: Rc<[String]>,
  ) -> Self {
    let mut child = self.create_child(doc_node);
    child.ns_qualifiers = qualifiers;
    child
  }

  pub fn create_child_method(
    &self,
    mut method_doc_node: DocNode,
    is_static: bool,
    method_kind: MethodKind,
  ) -> Self {
    let original_name = method_doc_node.name.clone();
    method_doc_node.name =
      qualify_drilldown_name(self.get_name(), &method_doc_node.name, is_static)
        .into_boxed_str();
    method_doc_node.declaration_kind = self.declaration_kind;

    let mut new_node = self.create_child(Rc::new(method_doc_node));
    new_node.drilldown_name = Some(original_name);
    new_node.kind_with_drilldown =
      DocNodeKindWithDrilldown::Method(method_kind);
    new_node
  }

  pub fn create_child_property(
    &self,
    mut property_doc_node: DocNode,
    is_static: bool,
  ) -> Self {
    let original_name = property_doc_node.name.clone();
    property_doc_node.name = qualify_drilldown_name(
      self.get_name(),
      &property_doc_node.name,
      is_static,
    )
    .into_boxed_str();
    property_doc_node.declaration_kind = self.declaration_kind;

    let mut new_node = self.create_child(Rc::new(property_doc_node));
    new_node.drilldown_name = Some(original_name);
    new_node.kind_with_drilldown = DocNodeKindWithDrilldown::Property;
    new_node
  }

  pub fn get_qualified_name(&self) -> String {
    if self.ns_qualifiers.is_empty() {
      self.get_name().to_string()
    } else {
      format!("{}.{}", self.ns_qualifiers.join("."), self.get_name())
    }
  }

  pub fn sub_qualifier(&self) -> Vec<String> {
    let mut ns_qualifiers = Vec::from(&*self.ns_qualifiers);
    ns_qualifiers.push(self.get_name().to_string());
    ns_qualifiers
  }

  pub fn is_internal(&self) -> bool {
    self.inner.declaration_kind == crate::node::DeclarationKind::Private
      || self
        .js_doc
        .tags
        .iter()
        .any(|tag| tag == &JsDocTag::Internal)
  }

  fn get_topmost_ancestor(&self) -> &DocNodeWithContext {
    match &self.parent {
      Some(parent_node) => parent_node.get_topmost_ancestor(),
      None => self,
    }
  }

  fn get_drilldown_symbols<'a>(
    &'a self,
  ) -> Option<Box<dyn Iterator<Item = DocNodeWithContext> + 'a>> {
    match &self.inner.def {
      DocNodeDef::Class { class_def } => Some(Box::new(
        class_def
          .methods
          .iter()
          .map(|method| {
            self.create_child_method(
              DocNode::function(
                method.name.clone(),
                false,
                method.location.clone(),
                self.declaration_kind,
                method.js_doc.clone(),
                method.function_def.clone(),
              ),
              method.is_static,
              method.kind,
            )
          })
          .chain(class_def.properties.iter().map(|property| {
            self.create_child_property(
              DocNode::from(property.clone()),
              property.is_static,
            )
          })),
      )),
      DocNodeDef::Interface { interface_def } => Some(Box::new(
        interface_def
          .methods
          .iter()
          .map(|method| {
            self.create_child_method(
              DocNode::from(method.clone()),
              true,
              method.kind,
            )
          })
          .chain(interface_def.properties.iter().map(|property| {
            self.create_child_property(DocNode::from(property.clone()), true)
          })),
      )),
      DocNodeDef::TypeAlias { type_alias_def } => {
        if let Some(ts_type_literal) =
          type_alias_def.ts_type.type_literal.as_ref()
        {
          Some(Box::new(
            ts_type_literal
              .methods
              .iter()
              .map(|method| {
                self.create_child_method(
                  DocNode::from(method.clone()),
                  true,
                  method.kind,
                )
              })
              .chain(ts_type_literal.properties.iter().map(|property| {
                self
                  .create_child_property(DocNode::from(property.clone()), true)
              })),
          ))
        } else {
          None
        }
      }
      DocNodeDef::Variable { variable_def } => {
        if let Some(ts_type_literal) = variable_def
          .ts_type
          .as_ref()
          .and_then(|ts_type| ts_type.type_literal.as_ref())
        {
          Some(Box::new(
            ts_type_literal
              .methods
              .iter()
              .map(|method| {
                self.create_child_method(
                  DocNode::from(method.clone()),
                  true,
                  method.kind,
                )
              })
              .chain(ts_type_literal.properties.iter().map(|property| {
                self
                  .create_child_property(DocNode::from(property.clone()), true)
              })),
          ))
        } else {
          None
        }
      }
      DocNodeDef::Function { .. } => None,
      DocNodeDef::Enum { .. } => None,
      DocNodeDef::Namespace { .. } => None,
      DocNodeDef::Import { .. } => None,
      DocNodeDef::ModuleDoc => None,
    }
  }
}

impl core::ops::Deref for DocNodeWithContext {
  type Target = DocNode;

  fn deref(&self) -> &Self::Target {
    &self.inner
  }
}

#[derive(Default, Debug, Eq, PartialEq)]
pub enum FileMode {
  #[default]
  Normal,
  Dts,
  Single,
  SingleDts,
}

pub fn generate(
  mut options: GenerateOptions,
  doc_nodes_by_url: IndexMap<ModuleSpecifier, Vec<DocNode>>,
) -> Result<HashMap<String, String>, serde_json::Error> {
  if doc_nodes_by_url.len() == 1 && options.main_entrypoint.is_none() {
    options.main_entrypoint =
      Some(doc_nodes_by_url.keys().next().unwrap().clone());
  }

  let file_mode = match (
    doc_nodes_by_url
      .keys()
      .all(|specifier| specifier.as_str().ends_with(".d.ts")),
    doc_nodes_by_url.len(),
  ) {
    (false, 1) => FileMode::Single,
    (false, _) => FileMode::Normal,
    (true, 1) => FileMode::SingleDts,
    (true, _) => FileMode::Dts,
  };

  let common_ancestor = find_common_ancestor(doc_nodes_by_url.keys(), true);
  let ctx =
    GenerateCtx::new(options, common_ancestor, file_mode, doc_nodes_by_url);
  let mut files = HashMap::new();

  // Index page
  {
    let (partitions_for_entrypoint_nodes, uses_categories) =
      if let Some(entrypoint) = ctx.main_entrypoint.as_ref() {
        let nodes = ctx.doc_nodes.get(entrypoint).unwrap();
        let categories = partition::partition_nodes_by_category(
          nodes.iter().map(Cow::Borrowed),
          ctx.file_mode == FileMode::SingleDts,
        );

        if categories.len() == 1 && categories.contains_key("Uncategorized") {
          (
            partition::partition_nodes_by_kind(
              nodes.iter().map(Cow::Borrowed),
              ctx.file_mode == FileMode::SingleDts,
            ),
            false,
          )
        } else {
          (categories, true)
        }
      } else {
        Default::default()
      };

    let index = pages::IndexCtx::new(
      &ctx,
      ctx.main_entrypoint.clone(),
      partitions_for_entrypoint_nodes,
      uses_categories,
    );

    files.insert(
      "./index.html".to_string(),
      ctx.render(pages::IndexCtx::TEMPLATE, &index),
    );
  }

  let all_doc_nodes = ctx
    .doc_nodes
    .values()
    .flatten()
    .cloned()
    .collect::<Vec<DocNodeWithContext>>();

  // All symbols (list of all symbols in all files)
  {
    let partitions_by_kind = partition::partition_nodes_by_entrypoint(
      all_doc_nodes.iter().map(Cow::Borrowed),
      true,
    );

    let all_symbols = pages::AllSymbolsCtx::new(&ctx, partitions_by_kind);

    files.insert(
      "./all_symbols.html".to_string(),
      ctx.render(pages::AllSymbolsCtx::TEMPLATE, &all_symbols),
    );
  }

  // Category pages
  if ctx.file_mode == FileMode::SingleDts {
    let categories = partition::partition_nodes_by_category(
      all_doc_nodes.iter().map(Cow::Borrowed),
      true,
    );

    if categories.len() != 1 {
      for (category, nodes) in &categories {
        let partitions = partition::partition_nodes_by_kind(
          nodes.iter().map(Cow::Borrowed),
          false,
        );

        let index = pages::IndexCtx::new_category(
          &ctx,
          category,
          partitions,
          &all_doc_nodes,
        );
        files.insert(
          format!("{}.html", util::slugify(category)),
          ctx.render(pages::IndexCtx::TEMPLATE, &index),
        );
      }
    }
  }

  // Pages for all discovered symbols
  {
    for (short_path, doc_nodes) in &ctx.doc_nodes {
      let doc_nodes_by_kind = partition::partition_nodes_by_kind(
        doc_nodes.iter().map(Cow::Borrowed),
        ctx.file_mode == FileMode::SingleDts,
      );

      let symbol_pages =
        generate_symbol_pages_for_module(&ctx, short_path, doc_nodes);

      files.extend(symbol_pages.into_iter().flat_map(|symbol_page| {
        match symbol_page {
          SymbolPage::Symbol {
            breadcrumbs_ctx,
            symbol_group_ctx,
            toc_ctx,
            categories_panel,
          } => {
            let root = ctx.resolve_path(
              UrlResolveKind::Symbol {
                file: short_path,
                symbol: &symbol_group_ctx.name,
              },
              UrlResolveKind::Root,
            );

            let mut title_parts = breadcrumbs_ctx.to_strings();
            title_parts.reverse();
            // contains the package name, which we already render in the head
            title_parts.pop();

            let html_head_ctx = pages::HtmlHeadCtx::new(
              &ctx,
              &root,
              Some(&title_parts.join(" - ")),
              Some(short_path),
            );

            let file_name =
              format!("{}/~/{}.html", short_path.path, symbol_group_ctx.name);

            let page_ctx = pages::SymbolPageCtx {
              html_head_ctx,
              symbol_group_ctx,
              breadcrumbs_ctx,
              toc_ctx,
              disable_search: ctx.disable_search,
              categories_panel,
            };

            let symbol_page =
              ctx.render(pages::SymbolPageCtx::TEMPLATE, &page_ctx);

            vec![(file_name, symbol_page)]
          }
          SymbolPage::Redirect {
            current_symbol,
            href,
          } => {
            let redirect = serde_json::json!({ "path": href });

            let file_name =
              format!("{}/~/{}.html", short_path.path, current_symbol);

            vec![(file_name, ctx.render("pages/redirect", &redirect))]
          }
        }
      }));

      if !short_path.is_main {
        let index = pages::IndexCtx::new(
          &ctx,
          Some(short_path.clone()),
          doc_nodes_by_kind,
          false,
        );

        files.insert(
          format!("{}/index.html", short_path.path),
          ctx.render(pages::IndexCtx::TEMPLATE, &index),
        );
      }
    }
  }

  files.insert(STYLESHEET_FILENAME.into(), STYLESHEET.into());
  files.insert(
    SEARCH_INDEX_FILENAME.into(),
    search::get_search_index_file(&ctx)?,
  );
  files.insert(SCRIPT_FILENAME.into(), SCRIPT_JS.into());

  files.insert(PAGE_STYLESHEET_FILENAME.into(), PAGE_STYLESHEET.into());
  files.insert(RESET_STYLESHEET_FILENAME.into(), RESET_STYLESHEET.into());
  files.insert(FUSE_FILENAME.into(), FUSE_JS.into());
  files.insert(SEARCH_FILENAME.into(), SEARCH_JS.into());
  #[cfg(feature = "comrak")]
  files.insert(
    comrak::COMRAK_STYLESHEET_FILENAME.into(),
    comrak::COMRAK_STYLESHEET.into(),
  );

  Ok(files)
}

pub fn find_common_ancestor<'a>(
  urls: impl Iterator<Item = &'a ModuleSpecifier>,
  single_file_is_common_ancestor: bool,
) -> Option<PathBuf> {
  let paths: Vec<PathBuf> = urls
    .filter_map(|url| {
      if url.scheme() == "file" {
        deno_path_util::url_to_file_path(url).ok()
      } else {
        None
      }
    })
    .collect();

  if paths.is_empty() || paths.len() == 1 && !single_file_is_common_ancestor {
    return None;
  }

  let shortest_path = paths
    .iter()
    .min_by_key(|path| path.components().count())
    .unwrap();

  let mut common_ancestor = PathBuf::new();

  for (index, component) in shortest_path.components().enumerate() {
    if paths.iter().all(|path| {
      path.components().count() > index
        && path.components().nth(index) == Some(component)
    }) {
      common_ancestor.push(component);
    } else {
      break;
    }
  }

  if common_ancestor.as_os_str().is_empty()
    || common_ancestor == PathBuf::from("/")
  {
    None
  } else {
    Some(common_ancestor)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[cfg(not(windows))]
  #[test]
  fn common_ancestor_root() {
    run_common_ancestor_test(
      &[
        "file:///bytes.ts",
        "file:///colors.ts",
        "file:///duration.ts",
        "file:///printf.ts",
      ],
      false,
      None,
    );
  }

  #[test]
  fn common_ancestor_single_file() {
    run_common_ancestor_test(&["file:///a/a.ts"], false, None);
  }

  #[test]
  fn common_ancestor_multiple_files() {
    run_common_ancestor_test(
      &["file:///a/a.ts", "file:///a/b.ts"],
      false,
      Some("file:///a/"),
    );
  }

  #[test]
  fn common_ancestor_single_file_single_mode() {
    run_common_ancestor_test(&["file:///a/a.ts"], true, Some("file:///a/a.ts"));
  }

  #[test]
  fn common_ancestor_multiple_file_single_mode() {
    run_common_ancestor_test(
      &["file:///a/a.ts", "file:///a/b.ts"],
      true,
      Some("file:///a/"),
    );
  }

  #[track_caller]
  fn run_common_ancestor_test(
    specifiers: &[&str],
    single_file_is_common_ancestor: bool,
    expected: Option<&str>,
  ) {
    let map = specifiers
      .iter()
      .map(|specifier| normalize_specifier(specifier))
      .collect::<Vec<_>>();

    let common_ancestor =
      find_common_ancestor(map.iter(), single_file_is_common_ancestor);
    assert_eq!(
      common_ancestor,
      expected.map(|e| normalize_specifier(e).to_file_path().unwrap()),
    );
  }

  fn normalize_specifier(specifier: &str) -> ModuleSpecifier {
    if cfg!(windows) {
      ModuleSpecifier::parse(&specifier.replace("file:///", "file:///c:/"))
        .unwrap()
    } else {
      ModuleSpecifier::parse(specifier).unwrap()
    }
  }
}

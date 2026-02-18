use crate::DocNode;
use crate::node::DocNodeDef;
use deno_ast::ModuleSpecifier;
use handlebars::Handlebars;
use handlebars::handlebars_helper;
use indexmap::IndexMap;
use serde::Deserialize;
use serde::Serialize;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;

pub mod diff;
pub mod jsdoc;
pub mod pages;
mod parameters;
pub mod partition;
mod render_context;
pub mod search;
mod symbols;
mod types;
mod usage;
pub mod util;

#[cfg(feature = "comrak")]
pub mod comrak;

use crate::html::pages::SymbolPage;
use crate::js_doc::JsDocTag;
use crate::parser::ParseOutput;
pub use diff::DiffIndex;
pub use diff::DiffStatus;
pub use pages::generate_symbol_pages_for_module;
pub use render_context::RenderContext;
pub use search::generate_search_index;
pub use symbols::AllSymbolsCtx;
pub use symbols::AllSymbolsEntrypointCtx;
pub use symbols::SymbolContentCtx;
pub use symbols::SymbolGroupCtx;
pub use symbols::namespace;
pub use usage::UsageComposer;
pub use usage::UsageComposerEntry;
pub use usage::UsageToMd;
pub use util::DocNodeKindCtx;
pub use util::HrefResolver;
pub use util::NamespacedGlobalSymbols;
pub use util::SectionHeaderCtx;
pub use util::ToCCtx;
pub use util::TopSymbolCtx;
pub use util::TopSymbolsCtx;
pub use util::UrlResolveKind;
pub use util::compute_namespaced_symbols;
pub use util::href_path_resolve;
pub use util::qualify_drilldown_name;

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

const DARKMODE_TOGGLE_JS: &str =
  include_str!("./templates/pages/darkmode_toggle.js");
const DARKMODE_TOGGLE_FILENAME: &str = "darkmode_toggle.js";

fn setup_hbs() -> Result<Handlebars<'static>, anyhow::Error> {
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
  reg.register_template_string(
    AllSymbolsCtx::TEMPLATE,
    include_str!("./templates/all_symbols.hbs"),
  )?;

  // pages
  reg.register_template_string(
    pages::HtmlHeadCtx::TEMPLATE,
    include_str!("./templates/pages/html_head.hbs"),
  )?;
  reg.register_template_string(
    pages::AllSymbolsPageCtx::TEMPLATE,
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
  reg.register_template_string(
    "icons/sun",
    include_str!("./templates/icons/sun.svg"),
  )?;
  reg.register_template_string(
    "icons/moon",
    include_str!("./templates/icons/moon.svg"),
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
  pub id_prefix: Option<String>,
  pub diff_only: bool,
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
  pub id_prefix: Option<String>,
  pub diff_only: bool,
  /// Index from Location to (depth, node) for fast reference resolution.
  reference_index: std::collections::HashMap<
    crate::Location,
    Vec<(usize, DocNodeWithContext)>,
  >,
  /// Optional diff index for annotating rendered output with diff status.
  pub diff: Option<DiffIndex>,
}

impl GenerateCtx {
  pub fn new(
    options: GenerateOptions,
    common_ancestor: Option<PathBuf>,
    file_mode: FileMode,
    doc_nodes_by_url: ParseOutput,
    diff: Option<crate::diff::DocDiff>,
  ) -> Result<Self, anyhow::Error> {
    let diff = diff.map(DiffIndex::new);

    let mut main_entrypoint = None;

    let mut doc_nodes = doc_nodes_by_url
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
            if &*node.name == "default"
              && let Some(default_rename) =
                options.default_symbol_map.as_ref().and_then(
                  |default_symbol_map| default_symbol_map.get(&short_path.path),
                )
            {
              node.name = default_rename.as_str().into();
            }

            // TODO(@crowlKats): support this in namespaces
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

            let diff_status = diff.as_ref().and_then(|d| {
              d.get_node_diff(
                &short_path.specifier,
                &node.name,
                node.def.to_kind(),
              )
              .map(|info| info.status.clone())
            });

            DocNodeWithContext {
              origin: short_path.clone(),
              ns_qualifiers: Rc::new([]),
              kind: DocNodeKind::from_node(&node),
              inner: Arc::new(node),
              drilldown_name: None,
              parent: None,
              namespace_children: None,
              qualified_name: std::cell::OnceCell::new(),
              diff_status,
            }
          })
          .map(|mut node| {
            fn handle_node(node: &mut DocNodeWithContext) {
              let children = if let Some(ns) = node.namespace_def() {
                let subqualifier: Rc<[String]> = node.sub_qualifier().into();
                Some(
                  ns.elements
                    .iter()
                    .map(|subnode| {
                      let mut child_node = node.create_namespace_child(
                        subnode.clone(),
                        subqualifier.clone(),
                      );
                      handle_node(&mut child_node);
                      child_node
                    })
                    .collect::<Vec<_>>(),
                )
              } else {
                None
              };

              node.namespace_children = children;
            }

            handle_node(&mut node);

            node
          })
          .collect::<Vec<_>>();

        (short_path, nodes)
      })
      .collect::<IndexMap<_, _>>();

    doc_nodes.sort_by_key(|a, _| !a.is_main);

    // Inject removed symbols from diff data into doc_nodes for listing
    if let Some(diff) = &diff {
      for (short_path, nodes) in &mut doc_nodes {
        if let Some(removed) = diff.get_removed_nodes(&short_path.specifier) {
          for node in removed {
            nodes.push(DocNodeWithContext {
              origin: short_path.clone(),
              ns_qualifiers: Rc::new([]),
              kind: DocNodeKind::from_node(node),
              inner: Arc::new(node.clone()),
              drilldown_name: None,
              parent: None,
              namespace_children: None,
              qualified_name: std::cell::OnceCell::new(),
              diff_status: Some(DiffStatus::Removed),
            });
          }
        }
      }
    }

    // Apply namespace diff data to namespace children
    if let Some(diff) = &diff {
      for (short_path, nodes) in &mut doc_nodes {
        for node in nodes.iter_mut() {
          apply_namespace_diff(node, diff, short_path);
        }
      }
    }

    let mut reference_index: std::collections::HashMap<
      crate::Location,
      Vec<(usize, DocNodeWithContext)>,
    > = std::collections::HashMap::new();
    {
      fn index_node(
        index: &mut std::collections::HashMap<
          crate::Location,
          Vec<(usize, DocNodeWithContext)>,
        >,
        node: &DocNodeWithContext,
        depth: usize,
      ) {
        index
          .entry(node.location.clone())
          .or_default()
          .push((depth, node.clone()));
        if matches!(node.def, DocNodeDef::Namespace { .. })
          && let Some(children) = &node.namespace_children
        {
          for child in children {
            index_node(index, child, depth + 1);
          }
        }
      }
      for nodes in doc_nodes.values() {
        for node in nodes {
          index_node(&mut reference_index, node, 0);
        }
      }
    }

    Ok(Self {
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
      id_prefix: options.id_prefix,
      diff_only: options.diff_only,
      reference_index,
      diff,
    })
  }

  pub fn create_basic(
    mut options: GenerateOptions,
    doc_nodes_by_url: ParseOutput,
  ) -> Result<Self, anyhow::Error> {
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

    GenerateCtx::new(
      options,
      common_ancestor,
      file_mode,
      doc_nodes_by_url,
      None,
    )
  }

  pub fn create_basic_with_diff(
    mut options: GenerateOptions,
    doc_nodes_by_url: ParseOutput,
    diff: crate::diff::DocDiff,
  ) -> Result<Self, anyhow::Error> {
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

    GenerateCtx::new(
      options,
      common_ancestor,
      file_mode,
      doc_nodes_by_url,
      Some(diff),
    )
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
    if let Some(symbol_redirect_map) = &self.symbol_redirect_map
      && let UrlResolveKind::Symbol { file, symbol } = target
      && let Some(path_map) = symbol_redirect_map.get(&file.path)
      && let Some(href) = path_map.get(symbol)
    {
      return href.clone();
    }

    self.href_resolver.resolve_path(current, target)
  }

  // TODO(@crowlKats): don't reference to another node, but redirect to it instead
  pub fn resolve_reference<'a>(
    &'a self,
    new_parent: Option<&'a DocNodeWithContext>,
    reference: &'a crate::Location,
  ) -> impl Iterator<Item = Cow<'a, DocNodeWithContext>> + 'a {
    fn strip_qualifiers(node: &mut DocNodeWithContext, depth: usize) {
      let ns_qualifiers = node.ns_qualifiers.to_vec();
      node.ns_qualifiers = ns_qualifiers[depth..].to_vec().into();
      node.qualified_name = std::cell::OnceCell::new();

      if let Some(children) = &mut node.namespace_children {
        for child in children {
          strip_qualifiers(child, depth);
        }
      }
    }

    let entries = self
      .reference_index
      .get(reference)
      .map(|v| v.as_slice())
      .unwrap_or(&[]);

    entries
      .iter()
      .map(|(depth, node)| {
        if *depth > 0 {
          let mut node = node.clone();
          strip_qualifiers(&mut node, *depth);
          Cow::Owned(node)
        } else {
          Cow::Borrowed(node)
        }
      })
      .map(move |node| {
        if let Some(parent) = new_parent {
          let mut node = node.into_owned();
          let mut ns_qualifiers = Vec::with_capacity(
            parent.ns_qualifiers.len() + node.ns_qualifiers.len(),
          );
          ns_qualifiers.extend(parent.sub_qualifier());

          fn handle_node(
            node: &mut DocNodeWithContext,
            ns_qualifiers: Vec<String>,
          ) {
            if let Some(children) = &mut node.namespace_children {
              for node in children {
                handle_node(node, ns_qualifiers.clone());
              }
            }

            let mut new_ns_qualifiers = ns_qualifiers;
            new_ns_qualifiers.extend(node.ns_qualifiers.iter().cloned());
            node.ns_qualifiers = new_ns_qualifiers.into();
            node.qualified_name = std::cell::OnceCell::new();
          }

          handle_node(&mut node, ns_qualifiers);

          Cow::Owned(node)
        } else {
          node
        }
      })
  }
}

/// Recursively apply namespace diff data to namespace children.
/// For each namespace node that has a `NamespaceDiff`, set `diff_status`
/// on children and inject removed elements.
fn apply_namespace_diff(
  node: &mut DocNodeWithContext,
  diff: &DiffIndex,
  short_path: &Rc<ShortPath>,
) {
  // Only process namespace nodes
  if !matches!(node.def, DocNodeDef::Namespace { .. }) {
    return;
  }

  // Look up the NamespaceDiff for this node from the DiffIndex
  let ns_diff = diff
    .get_node_diff(&short_path.specifier, &node.name, node.def.to_kind())
    .and_then(|info| info.diff.as_ref())
    .and_then(|node_diff| node_diff.def_changes.as_ref())
    .and_then(|def_diff| {
      if let crate::diff::DocNodeDefDiff::Namespace(ns_diff) = def_diff {
        Some(ns_diff.clone())
      } else {
        None
      }
    });

  apply_namespace_diff_inner(node, ns_diff.as_ref(), short_path);
}

/// Inner recursive function that applies a `NamespaceDiff` to a namespace
/// node's children, and recurses into nested namespaces using their own
/// `NamespaceDiff` from the parent's `modified_elements`.
fn apply_namespace_diff_inner(
  node: &mut DocNodeWithContext,
  ns_diff: Option<&crate::diff::NamespaceDiff>,
  short_path: &Rc<ShortPath>,
) {
  if let Some(ns_diff) = ns_diff {
    // Pre-compute values needed for removed element injection
    // before taking mutable borrow on children
    let subqualifier: Rc<[String]> = node.sub_qualifier().into();
    let node_snapshot = node.clone();

    if let Some(children) = &mut node.namespace_children {
      // Build lookup sets for added and modified elements
      let added_names: std::collections::HashSet<(
        String,
        crate::node::DocNodeKind,
      )> = ns_diff
        .added_elements
        .iter()
        .map(|n| (n.name.to_string(), n.def.to_kind()))
        .collect();

      let modified_map: HashMap<
        (String, crate::node::DocNodeKind),
        &crate::diff::DocNodeDiff,
      > = ns_diff
        .modified_elements
        .iter()
        .map(|d| ((d.name.to_string(), d.kind), d))
        .collect();

      // Set diff_status on existing children
      for child in children.iter_mut() {
        let kind = child.def.to_kind();
        let name = child.name.to_string();
        let key = (name, kind);
        if added_names.contains(&key) {
          child.diff_status = Some(DiffStatus::Added);
        } else if let Some(node_diff) = modified_map.get(&key) {
          child.diff_status = if let Some(name_change) = &node_diff.name_change
          {
            Some(DiffStatus::Renamed {
              old_name: name_change.old.to_string(),
            })
          } else {
            Some(DiffStatus::Modified)
          };

          // If child is a namespace, recurse with its own NamespaceDiff
          if matches!(child.def, DocNodeDef::Namespace { .. }) {
            let child_ns_diff =
              node_diff.def_changes.as_ref().and_then(|def_diff| {
                if let crate::diff::DocNodeDefDiff::Namespace(ns_diff) =
                  def_diff
                {
                  Some(ns_diff)
                } else {
                  None
                }
              });
            apply_namespace_diff_inner(child, child_ns_diff, short_path);
          }
        }
      }

      // Inject removed elements
      for removed_node in &ns_diff.removed_elements {
        children.push(DocNodeWithContext {
          origin: short_path.clone(),
          ns_qualifiers: subqualifier.clone(),
          kind: DocNodeKind::from_node(removed_node),
          inner: removed_node.clone(),
          drilldown_name: None,
          parent: Some(Box::new(node_snapshot.clone())),
          namespace_children: None,
          qualified_name: std::cell::OnceCell::new(),
          diff_status: Some(DiffStatus::Removed),
        });
      }
    }
  } else {
    // Even if this namespace doesn't have a diff, recurse into children
    // in case nested namespaces do (shouldn't happen but be safe)
    if let Some(children) = &mut node.namespace_children {
      for child in children.iter_mut() {
        if matches!(child.def, DocNodeDef::Namespace { .. }) {
          apply_namespace_diff_inner(child, None, short_path);
        }
      }
    }
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

    let stripped_path = common_ancestor
      .and_then(|ancestor| url_file_path.strip_prefix(ancestor).ok())
      .unwrap_or(&url_file_path);

    let path = stripped_path.to_string_lossy().to_string();
    let path = path.strip_prefix('/').unwrap_or(&path).to_string();

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

  pub fn as_resolve_kind(&self) -> UrlResolveKind<'_> {
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

#[derive(
  Debug,
  PartialEq,
  Eq,
  Hash,
  Clone,
  Copy,
  Serialize,
  Deserialize,
  Ord,
  PartialOrd,
)]
pub enum MethodKind {
  Method,
  Getter,
  Setter,
}

impl From<deno_ast::swc::ast::MethodKind> for MethodKind {
  fn from(value: deno_ast::swc::ast::MethodKind) -> Self {
    match value {
      deno_ast::swc::ast::MethodKind::Method => Self::Method,
      deno_ast::swc::ast::MethodKind::Getter => Self::Getter,
      deno_ast::swc::ast::MethodKind::Setter => Self::Setter,
    }
  }
}

#[derive(
  Debug,
  PartialEq,
  Eq,
  Hash,
  Clone,
  Copy,
  Serialize,
  Deserialize,
  Ord,
  PartialOrd,
)]
pub enum DocNodeKind {
  Property,
  Method(MethodKind),
  Class,
  Enum,
  Function,
  Import,
  Interface,
  ModuleDoc,
  Namespace,
  Reference,
  TypeAlias,
  Variable,
}

impl DocNodeKind {
  fn from_node(node: &DocNode) -> Self {
    match node.def {
      DocNodeDef::Function { .. } => Self::Function,
      DocNodeDef::Variable { .. } => Self::Variable,
      DocNodeDef::Enum { .. } => Self::Enum,
      DocNodeDef::Class { .. } => Self::Class,
      DocNodeDef::TypeAlias { .. } => Self::TypeAlias,
      DocNodeDef::Namespace { .. } => Self::Namespace,
      DocNodeDef::Interface { .. } => Self::Interface,
      DocNodeDef::Import { .. } => Self::Import,
      DocNodeDef::ModuleDoc => Self::ModuleDoc,
      DocNodeDef::Reference { .. } => Self::Reference,
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
  pub kind: DocNodeKind,
  pub inner: Arc<DocNode>,
  pub drilldown_name: Option<Box<str>>,
  pub parent: Option<Box<DocNodeWithContext>>,
  pub namespace_children: Option<Vec<DocNodeWithContext>>,
  #[serde(skip, default)]
  qualified_name: std::cell::OnceCell<String>,
  /// Optional diff status for this node (set when diff data is provided).
  #[serde(skip_serializing_if = "Option::is_none")]
  pub diff_status: Option<DiffStatus>,
}

impl DocNodeWithContext {
  pub fn create_child(&self, doc_node: Arc<DocNode>) -> Self {
    DocNodeWithContext {
      origin: self.origin.clone(),
      ns_qualifiers: self.ns_qualifiers.clone(),
      kind: DocNodeKind::from_node(&doc_node),
      inner: doc_node,
      drilldown_name: None,
      parent: Some(Box::new(self.clone())),
      namespace_children: None,
      qualified_name: std::cell::OnceCell::new(),
      diff_status: None,
    }
  }

  pub fn create_namespace_child(
    &self,
    doc_node: Arc<DocNode>,
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
    method_kind: deno_ast::swc::ast::MethodKind,
  ) -> Self {
    let original_name = method_doc_node.name.clone();
    method_doc_node.name =
      qualify_drilldown_name(self.get_name(), &method_doc_node.name, is_static)
        .into_boxed_str();
    method_doc_node.declaration_kind = self.declaration_kind;

    let mut new_node = self.create_child(Arc::new(method_doc_node));
    new_node.drilldown_name = Some(original_name);
    new_node.kind = DocNodeKind::Method(method_kind.into());
    new_node.diff_status = self.diff_status.clone();
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

    let mut new_node = self.create_child(Arc::new(property_doc_node));
    new_node.drilldown_name = Some(original_name);
    new_node.kind = DocNodeKind::Property;
    new_node.diff_status = self.diff_status.clone();
    new_node
  }

  pub fn get_qualified_name(&self) -> &str {
    self.qualified_name.get_or_init(|| {
      if self.ns_qualifiers.is_empty() {
        self.get_name().to_string()
      } else {
        format!("{}.{}", self.ns_qualifiers.join("."), self.get_name())
      }
    })
  }

  pub fn sub_qualifier(&self) -> Vec<String> {
    let mut ns_qualifiers = Vec::from(&*self.ns_qualifiers);
    ns_qualifiers.push(self.get_name().to_string());
    ns_qualifiers
  }

  pub fn is_internal(&self, ctx: &GenerateCtx) -> bool {
    (self.inner.declaration_kind == crate::node::DeclarationKind::Private
      && !matches!(ctx.file_mode, FileMode::SingleDts | FileMode::Dts))
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
      DocNodeDef::Reference { .. } => None,
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
  ctx: GenerateCtx,
) -> Result<HashMap<String, String>, anyhow::Error> {
  let mut files = HashMap::new();

  // Index page
  {
    let (partitions_for_entrypoint_nodes, uses_categories) =
      if let Some(entrypoint) = ctx.main_entrypoint.as_ref() {
        let nodes = ctx.doc_nodes.get(entrypoint).unwrap();
        let categories = partition::partition_nodes_by_category(
          &ctx,
          nodes.iter().map(Cow::Borrowed),
          ctx.file_mode == FileMode::SingleDts,
        );

        if categories.len() == 1 && categories.contains_key("Uncategorized") {
          (
            partition::partition_nodes_by_kind(
              &ctx,
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

  // All symbols (list of all symbols in all files)
  {
    let all_symbols = pages::AllSymbolsPageCtx::new(&ctx);

    files.insert(
      "./all_symbols.html".to_string(),
      ctx.render(pages::AllSymbolsPageCtx::TEMPLATE, &all_symbols),
    );
  }

  // Category pages
  if ctx.file_mode == FileMode::SingleDts {
    let all_doc_nodes = ctx
      .doc_nodes
      .values()
      .flatten()
      .cloned()
      .collect::<Vec<DocNodeWithContext>>();

    let categories = partition::partition_nodes_by_category(
      &ctx,
      all_doc_nodes.iter().map(Cow::Borrowed),
      true,
    );

    if categories.len() != 1 {
      for (category, nodes) in &categories {
        let partitions = partition::partition_nodes_by_kind(
          &ctx,
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
        &ctx,
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
            let redirect =
              serde_json::json!({ "kind": "redirect", "path": href });

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
  files.insert(DARKMODE_TOGGLE_FILENAME.into(), DARKMODE_TOGGLE_JS.into());
  #[cfg(feature = "comrak")]
  files.insert(
    comrak::COMRAK_STYLESHEET_FILENAME.into(),
    comrak::COMRAK_STYLESHEET.into(),
  );

  Ok(files)
}

pub fn generate_json(
  ctx: GenerateCtx,
) -> Result<HashMap<String, serde_json::Value>, anyhow::Error> {
  let mut files = HashMap::new();

  let diff_only = ctx.diff_only;

  // Index page
  {
    let (partitions_for_entrypoint_nodes, uses_categories) =
      if let Some(entrypoint) = ctx.main_entrypoint.as_ref() {
        let nodes = ctx.doc_nodes.get(entrypoint).unwrap();
        let categories = partition::partition_nodes_by_category(
          &ctx,
          nodes.iter().map(Cow::Borrowed),
          ctx.file_mode == FileMode::SingleDts,
        );

        if categories.len() == 1 && categories.contains_key("Uncategorized") {
          (
            partition::partition_nodes_by_kind(
              &ctx,
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

    if !diff_only
      || index
        .overview
        .as_ref()
        .is_some_and(|o| !o.sections.is_empty())
      || index
        .module_doc
        .as_ref()
        .is_some_and(|md| !md.sections.sections.is_empty())
    {
      files.insert("./index.json".to_string(), serde_json::to_value(index)?);
    }
  }

  let all_doc_nodes = ctx
    .doc_nodes
    .values()
    .flatten()
    .cloned()
    .collect::<Vec<DocNodeWithContext>>();

  // All symbols (list of all symbols in all files)
  {
    let all_symbols = pages::AllSymbolsPageCtx::new(&ctx);

    if !diff_only || !all_symbols.content.entrypoints.is_empty() {
      files.insert(
        "./all_symbols.json".to_string(),
        serde_json::to_value(all_symbols)?,
      );
    }
  }

  // Category pages
  if ctx.file_mode == FileMode::SingleDts {
    let categories = partition::partition_nodes_by_category(
      &ctx,
      all_doc_nodes.iter().map(Cow::Borrowed),
      true,
    );

    if categories.len() != 1 {
      for (category, nodes) in &categories {
        let partitions = partition::partition_nodes_by_kind(
          &ctx,
          nodes.iter().map(Cow::Borrowed),
          false,
        );

        let index = pages::IndexCtx::new_category(
          &ctx,
          category,
          partitions,
          &all_doc_nodes,
        );

        if diff_only
          && index
            .overview
            .as_ref()
            .is_none_or(|o| !o.sections.is_empty())
        {
          continue;
        }

        files.insert(
          format!("{}.json", util::slugify(category)),
          serde_json::to_value(index)?,
        );
      }
    }
  }

  // Pages for all discovered symbols
  {
    for (short_path, doc_nodes) in &ctx.doc_nodes {
      let doc_nodes_by_kind = partition::partition_nodes_by_kind(
        &ctx,
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
            // In diff_only mode, skip unchanged symbols
            if diff_only && symbol_group_ctx.diff_status.is_none() {
              return vec![];
            }

            let mut symbol_group_ctx = symbol_group_ctx;
            if diff_only {
              symbol_group_ctx.strip_unchanged_tags();
            }

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
              format!("{}/~/{}.json", short_path.path, symbol_group_ctx.name);

            let page_ctx = pages::SymbolPageCtx {
              html_head_ctx,
              symbol_group_ctx,
              breadcrumbs_ctx,
              toc_ctx,
              disable_search: ctx.disable_search,
              categories_panel,
            };

            vec![(file_name, serde_json::to_value(page_ctx).unwrap())]
          }
          SymbolPage::Redirect {
            current_symbol,
            href,
          } => {
            // Skip redirects in diff_only mode
            if diff_only {
              return vec![];
            }

            let redirect = serde_json::json!({ "path": href });

            let file_name =
              format!("{}/~/{}.json", short_path.path, current_symbol);

            vec![(file_name, redirect)]
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

        if diff_only
          && index
            .overview
            .as_ref()
            .is_none_or(|o| !o.sections.is_empty())
          && index
            .module_doc
            .as_ref()
            .is_none_or(|md| !md.sections.sections.is_empty())
        {
          continue;
        }

        files.insert(
          format!("{}/index.json", short_path.path),
          serde_json::to_value(index)?,
        );
      }
    }
  }

  // Skip search index in diff_only mode
  if !diff_only {
    files.insert("search.json".into(), generate_search_index(&ctx));
  }

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

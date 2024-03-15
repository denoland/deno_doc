use super::sidepanels;
use super::sidepanels::SidepanelCtx;
use super::symbols::SymbolContentCtx;
use super::util::{qualify_drilldown_name, BreadcrumbsCtx};
use super::DocNodeWithContext;
use super::GenerateCtx;
use super::RenderContext;
use super::ShortPath;
use super::SymbolGroupCtx;
use super::UrlResolveKind;
use std::borrow::Cow;
use std::rc::Rc;

use super::FUSE_FILENAME;
use super::PAGE_STYLESHEET_FILENAME;
use super::SCRIPT_FILENAME;
use super::SEARCH_FILENAME;
use super::SEARCH_INDEX_FILENAME;
use super::STYLESHEET_FILENAME;

use crate::function::FunctionDef;
use crate::variable::VariableDef;
use crate::DocNode;
use crate::DocNodeKind;
use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use serde::Serialize;

#[derive(Debug, Serialize, Clone)]
pub struct HtmlHeadCtx {
  title: String,
  current_file: String,
  stylesheet_url: String,
  page_stylesheet_url: String,
  url_search_index: String,
  script_js: String,
  fuse_js: String,
  url_search: String,
}

impl HtmlHeadCtx {
  pub fn new(
    root: &str,
    page: &str,
    package_name: Option<&String>,
    current_file: Option<ShortPath>,
  ) -> Self {
    Self {
      title: format!(
        "{page} - {}documentation",
        package_name
          .map(|package_name| format!("{package_name} "))
          .unwrap_or_default()
      ),
      current_file: current_file
        .as_ref()
        .map(|current_file| current_file.as_str())
        .unwrap_or_default()
        .to_string(),
      stylesheet_url: format!("{root}{STYLESHEET_FILENAME}"),
      page_stylesheet_url: format!("{root}{PAGE_STYLESHEET_FILENAME}"),
      url_search_index: format!("{root}{SEARCH_INDEX_FILENAME}"),
      script_js: format!("{root}{SCRIPT_FILENAME}"),
      fuse_js: format!("{root}{FUSE_FILENAME}"),
      url_search: format!("{root}{SEARCH_FILENAME}"),
    }
  }
}

#[derive(Debug, Serialize, Clone)]
struct IndexCtx {
  html_head_ctx: HtmlHeadCtx,
  sidepanel_ctx: sidepanels::IndexSidepanelCtx,
  module_doc: Option<super::jsdoc::ModuleDocCtx>,
  breadcrumbs_ctx: BreadcrumbsCtx,
}

pub fn render_index(
  ctx: &GenerateCtx,
  specifier: Option<&ModuleSpecifier>,
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNodeWithContext>>,
  partitions: IndexMap<String, Vec<DocNodeWithContext>>,
  file: Option<ShortPath>,
) -> String {
  let sidepanel_ctx = sidepanels::IndexSidepanelCtx::new(
    ctx,
    specifier,
    doc_nodes_by_url,
    partitions,
    file.as_ref(),
  );

  let short_path = specifier.map(|specifier| ctx.url_to_short_path(specifier));

  // will be default on index page with no main entrypoint
  let default = vec![];
  let doc_nodes = specifier
    .or(ctx.main_entrypoint.as_ref())
    .and_then(|specifier| doc_nodes_by_url.get(specifier))
    .unwrap_or(&default);

  let render_ctx = RenderContext::new(
    ctx,
    doc_nodes,
    short_path
      .as_ref()
      .map_or(UrlResolveKind::Root, UrlResolveKind::File),
    specifier,
  );

  let module_doc = specifier.map(|specifier| {
    super::jsdoc::ModuleDocCtx::new(&render_ctx, specifier, doc_nodes_by_url)
  });

  let root = ctx.href_resolver.resolve_path(
    file
      .as_ref()
      .map_or(UrlResolveKind::Root, UrlResolveKind::File),
    UrlResolveKind::Root,
  );

  let html_head_ctx =
    HtmlHeadCtx::new(&root, "Index", ctx.package_name.as_ref(), None);

  let index_ctx = IndexCtx {
    html_head_ctx,
    sidepanel_ctx,
    module_doc,
    breadcrumbs_ctx: render_ctx.get_breadcrumbs(),
  };

  render_ctx
    .ctx
    .hbs
    .render("pages/index", &index_ctx)
    .unwrap()
}

#[derive(Serialize)]
struct AllSymbolsCtx {
  html_head_ctx: HtmlHeadCtx,
  content: SymbolContentCtx,
  breadcrumbs_ctx: BreadcrumbsCtx,
}

pub(crate) fn render_all_symbols_page(
  ctx: &GenerateCtx,
  partitions: IndexMap<DocNodeKind, Vec<DocNodeWithContext>>,
) -> String {
  // TODO(@crowlKats): handle doc_nodes in all symbols page for each symbol
  let render_ctx =
    RenderContext::new(ctx, &[], UrlResolveKind::AllSymbols, None);

  let sections = super::namespace::render_namespace(&render_ctx, partitions);

  let html_head_ctx =
    HtmlHeadCtx::new("./", "All Symbols", ctx.package_name.as_ref(), None);
  let all_symbols_ctx = AllSymbolsCtx {
    html_head_ctx,
    content: SymbolContentCtx {
      id: String::new(),
      sections,
      docs: None,
    },
    breadcrumbs_ctx: render_ctx.get_breadcrumbs(),
  };

  render_ctx
    .ctx
    .hbs
    .render("pages/all_symbols", &all_symbols_ctx)
    .unwrap()
}

pub fn generate_symbol_pages_for_module(
  ctx: &GenerateCtx,
  current_specifier: &ModuleSpecifier,
  short_path: &ShortPath,
  partitions_for_nodes: &IndexMap<String, Vec<DocNodeWithContext>>,
  doc_nodes: &[DocNodeWithContext],
) -> Vec<(BreadcrumbsCtx, SidepanelCtx, SymbolGroupCtx)> {
  let mut name_partitions =
    super::partition::partition_nodes_by_name(doc_nodes);

  let mut drilldown_partitions = IndexMap::new();
  for (name, doc_nodes) in &name_partitions {
    if doc_nodes[0].kind == DocNodeKind::Class {
      let class = doc_nodes[0].class_def.as_ref().unwrap();
      let method_nodes = class
        .methods
        .iter()
        .map(|method| DocNodeWithContext {
          origin: doc_nodes[0].origin.clone(),
          ns_qualifiers: Rc::new(vec![]),
          inner: Cow::Owned(DocNode::function(
            qualify_drilldown_name(name, &method.name, method.is_static),
            method.location.clone(),
            doc_nodes[0].declaration_kind,
            method.js_doc.clone(),
            method.function_def.clone(),
          )),
        })
        .collect::<Vec<_>>();

      drilldown_partitions
        .extend(super::partition::partition_nodes_by_name(&method_nodes));

      let property_nodes = class
        .properties
        .iter()
        .map(|property| DocNodeWithContext {
          origin: doc_nodes[0].origin.clone(),
          ns_qualifiers: Rc::new(vec![]),
          inner: Cow::Owned(DocNode::variable(
            qualify_drilldown_name(name, &property.name, property.is_static),
            property.location.clone(),
            doc_nodes[0].declaration_kind,
            property.js_doc.clone(),
            VariableDef {
              ts_type: property.ts_type.clone(),
              kind: deno_ast::swc::ast::VarDeclKind::Const,
            },
          )),
        })
        .collect::<Vec<_>>();

      drilldown_partitions
        .extend(super::partition::partition_nodes_by_name(&property_nodes));
    } else if doc_nodes[0].kind == DocNodeKind::Interface {
      let interface = doc_nodes[0].interface_def.as_ref().unwrap();
      let method_nodes = interface
        .methods
        .iter()
        .map(|method| DocNodeWithContext {
          origin: doc_nodes[0].origin.clone(),
          ns_qualifiers: Rc::new(vec![]),
          inner: Cow::Owned(DocNode::function(
            qualify_drilldown_name(name, &method.name, false),
            method.location.clone(),
            doc_nodes[0].declaration_kind,
            method.js_doc.clone(),
            FunctionDef {
              def_name: None,
              params: method.params.clone(),
              return_type: method.return_type.clone(),
              has_body: false,
              is_async: false,
              is_generator: false,
              type_params: method.type_params.clone(),
              decorators: vec![],
            },
          )),
        })
        .collect::<Vec<_>>();

      drilldown_partitions
        .extend(super::partition::partition_nodes_by_name(&method_nodes));

      let property_nodes = interface
        .properties
        .iter()
        .map(|property| DocNodeWithContext {
          origin: doc_nodes[0].origin.clone(),
          ns_qualifiers: Rc::new(vec![]),
          inner: Cow::Owned(DocNode::variable(
            qualify_drilldown_name(name, &property.name, false),
            property.location.clone(),
            doc_nodes[0].declaration_kind,
            property.js_doc.clone(),
            VariableDef {
              ts_type: property.ts_type.clone(),
              kind: deno_ast::swc::ast::VarDeclKind::Const,
            },
          )),
        })
        .collect::<Vec<_>>();

      drilldown_partitions
        .extend(super::partition::partition_nodes_by_name(&property_nodes));
    }
  }

  name_partitions.extend(drilldown_partitions);

  generate_symbol_pages_inner(
    ctx,
    doc_nodes,
    partitions_for_nodes,
    name_partitions,
    current_specifier,
    short_path,
    vec![],
  )
}

/*pub fn generate_symbol_page(
  ctx: &GenerateCtx,
  current_specifier: &ModuleSpecifier,
  short_path: &ShortPath,
  partitions_for_nodes: &IndexMap<String, Vec<DocNodeWithContext>>,
  doc_nodes_for_module: &[DocNodeWithContext],
  name: &str,
) -> Option<(BreadcrumbsCtx, SidepanelCtx, SymbolGroupCtx)> {
  let mut name_parts = name.split('.').peekable();

  let mut doc_nodes = doc_nodes_for_module;

  let mut namespace_paths = vec![];

  let doc_nodes = loop {
    let next_part = name_parts.next()?;
    let mut nodes = doc_nodes.iter().filter(|node| {
      if matches!(
        node.inner.kind,
        DocNodeKind::ModuleDoc | DocNodeKind::Import
      ) || node.inner.declaration_kind
        == crate::node::DeclarationKind::Private
      {
        return false;
      }
      node.get_name() == next_part
    });
    if name_parts.peek().is_none() {
      break nodes.cloned().collect::<Vec<_>>();
    }
    namespace_paths.push(next_part);
    if let Some(namespace) =
      nodes.find(|node| matches!(node.inner.kind, DocNodeKind::Namespace))
    {
      let namespace = namespace.inner.namespace_def.as_ref().unwrap();
      doc_nodes = &namespace.elements;
    } else {
      return None;
    }
  };

  if doc_nodes.is_empty() {
    return None;
  }

  let sidepanel_ctx =
    SidepanelCtx::new(ctx, partitions_for_nodes, short_path, name);

  let (breadcrumbs_ctx, symbol_group_ctx) = render_symbol_page(
    ctx,
    doc_nodes_for_module,
    current_specifier,
    short_path,
    &namespace_paths,
    name,
    &doc_nodes,
  );

  Some((breadcrumbs_ctx, sidepanel_ctx, symbol_group_ctx))
}*/

fn generate_symbol_pages_inner(
  ctx: &GenerateCtx,
  doc_nodes_for_module: &[DocNodeWithContext],
  partitions_for_nodes: &IndexMap<String, Vec<DocNodeWithContext>>,
  name_partitions: IndexMap<String, Vec<DocNodeWithContext>>,
  current_specifier: &ModuleSpecifier,
  short_path: &ShortPath,
  namespace_paths: Vec<&str>,
) -> Vec<(BreadcrumbsCtx, SidepanelCtx, SymbolGroupCtx)> {
  let mut generated_pages =
    Vec::with_capacity(name_partitions.values().len() * 2);

  for (name, doc_nodes) in name_partitions.iter() {
    let namespaced_name = if namespace_paths.is_empty() {
      name.to_owned()
    } else {
      format!("{}.{name}", namespace_paths.join("."))
    };

    let sidepanel_ctx = SidepanelCtx::new(
      ctx,
      partitions_for_nodes,
      short_path,
      &namespaced_name,
    );

    let (breadcrumbs_ctx, symbol_group_ctx) = render_symbol_page(
      ctx,
      doc_nodes_for_module,
      current_specifier,
      short_path,
      &namespace_paths,
      &namespaced_name,
      doc_nodes,
    );

    generated_pages.push((breadcrumbs_ctx, sidepanel_ctx, symbol_group_ctx));

    if let Some(doc_node) = doc_nodes
      .iter()
      .find(|doc_node| doc_node.kind == DocNodeKind::Namespace)
    {
      let namespace = doc_node.namespace_def.as_ref().unwrap();
      let namespace_elements = &namespace.elements;

      let namespace_name_partitions = super::partition::partition_nodes_by_name(
        &namespace_elements
          .iter()
          .map(|node| DocNodeWithContext {
            origin: doc_node.origin.clone(),
            ns_qualifiers: Rc::new(vec![]), // TODO
            inner: Cow::Borrowed(node),
          })
          .collect::<Vec<_>>(),
      );

      let namespace_paths = {
        let mut ns_paths = namespace_paths.clone();
        ns_paths.push(name);
        ns_paths
      };

      let generated = generate_symbol_pages_inner(
        ctx,
        doc_nodes_for_module,
        partitions_for_nodes,
        namespace_name_partitions,
        current_specifier,
        short_path,
        namespace_paths,
      );
      generated_pages.extend_from_slice(&generated);
    }
  }

  generated_pages
}

#[derive(Debug, Serialize)]
pub struct PageCtx {
  pub html_head_ctx: HtmlHeadCtx,
  pub sidepanel_ctx: SidepanelCtx,
  pub symbol_group_ctx: SymbolGroupCtx,
  pub breadcrumbs_ctx: BreadcrumbsCtx,
}

fn render_symbol_page(
  ctx: &GenerateCtx,
  doc_nodes_for_module: &[DocNodeWithContext],
  current_specifier: &ModuleSpecifier,
  short_path: &ShortPath,
  namespace_paths: &[&str],
  namespaced_name: &str,
  doc_nodes: &[DocNodeWithContext],
) -> (BreadcrumbsCtx, SymbolGroupCtx) {
  let mut render_ctx = RenderContext::new(
    ctx,
    doc_nodes_for_module,
    UrlResolveKind::Symbol {
      file: short_path,
      symbol: namespaced_name,
    },
    Some(current_specifier),
  );
  if !namespace_paths.is_empty() {
    render_ctx = render_ctx.with_namespace(namespace_paths.to_vec())
  }

  // NOTE: `doc_nodes` should be sorted at this point.
  let symbol_group_ctx =
    SymbolGroupCtx::new(&render_ctx, doc_nodes, namespaced_name);

  (render_ctx.get_breadcrumbs(), symbol_group_ctx)
}

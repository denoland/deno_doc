use crate::html::DiffStatus;
use crate::html::DocNodeWithContext;
use crate::html::RenderContext;
use crate::html::diff::is_symbol_added;
use crate::html::diff::is_symbol_removed;
use crate::html::jsdoc::ModuleDocCtx;
use crate::html::types::render_type_def;
use crate::html::usage::UsagesCtx;
use crate::html::util::AnchorCtx;
use crate::html::util::SectionContentCtx;
use crate::html::util::SectionCtx;
use crate::html::util::Tag;
use crate::html::{DocNodeKind, UrlResolveKind};
use crate::js_doc::JsDocTag;
use crate::node::DocNodeDef;
use indexmap::IndexMap;
use serde::Deserialize;
use serde::Serialize;
use std::borrow::Cow;
use std::collections::HashSet;

pub mod class;
pub mod r#enum;
pub mod function;
pub mod interface;
pub mod namespace;
pub mod type_alias;
pub mod variable;

#[derive(Debug, Serialize, Deserialize, Clone)]
struct SymbolCtx {
  kind: super::util::DocNodeKindCtx,
  usage: Option<UsagesCtx>,
  tags: Vec<super::util::TagCtx>,
  subtitle: Option<DocBlockSubtitleCtx>,
  content: Vec<SymbolInnerCtx>,
  deprecated: Option<String>,
  source_href: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  diff_status: Option<DiffStatus>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct SymbolGroupCtx {
  pub name: String,
  symbols: Vec<SymbolCtx>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub diff_status: Option<DiffStatus>,
}

impl SymbolGroupCtx {
  pub const TEMPLATE: &'static str = "symbol_group";

  pub fn new(
    ctx: &RenderContext,
    doc_nodes: &[DocNodeWithContext],
    name: &str,
  ) -> Self {
    let mut split_nodes =
      IndexMap::<DocNodeKind, Vec<DocNodeWithContext>>::default();

    for doc_node in doc_nodes {
      if matches!(doc_node.def, DocNodeDef::Import { .. }) {
        continue;
      }

      split_nodes
        .entry(doc_node.kind)
        .or_insert(vec![])
        .push(doc_node.clone());
    }

    split_nodes.sort_keys();

    let mut symbols = split_nodes
      .values()
      .map(|doc_nodes| {
        let all_deprecated =
          super::util::all_deprecated(&doc_nodes.iter().collect::<Vec<_>>());

        let mut tags = indexmap::IndexSet::new();

        if doc_nodes.iter().any(|node| {
          node
            .js_doc
            .tags
            .iter()
            .any(|tag| matches!(tag, JsDocTag::Experimental))
        }) {
          tags.insert(Tag::Unstable);
        }

        let mut permissions = doc_nodes
          .iter()
          .flat_map(|node| {
            node
              .js_doc
              .tags
              .iter()
              .filter_map(|tag| {
                if let JsDocTag::Tags { tags } = tag {
                  Some(tags.iter().filter_map(|tag| {
                    if tag.starts_with("allow-") {
                      Some(tag.to_owned())
                    } else {
                      None
                    }
                  }))
                } else {
                  None
                }
              })
              .flatten()
          })
          .collect::<indexmap::IndexSet<_>>();

        if !permissions.is_empty() {
          permissions.sort();
          tags.insert(Tag::Permissions(permissions.into_iter().collect()));
        }

        if doc_nodes[0].is_internal(ctx.ctx) {
          tags.insert(Tag::Private);
        }

        let deprecated = if all_deprecated
          && !(matches!(doc_nodes[0].def, DocNodeDef::Function { .. })
            && doc_nodes.len() == 1)
        {
          doc_nodes[0].js_doc.tags.iter().find_map(|tag| {
            if let JsDocTag::Deprecated { doc } = tag {
              Some(
                doc
                  .as_ref()
                  .map(|doc| {
                    crate::html::jsdoc::render_markdown(ctx, doc, true)
                  })
                  .unwrap_or_default(),
              )
            } else {
              None
            }
          })
        } else {
          None
        };

        let usage = matches!(
          ctx.ctx.file_mode,
          super::FileMode::SingleDts | super::FileMode::Dts
        )
        .then(|| UsagesCtx::new(ctx, doc_nodes))
        .flatten();

        let old_tags = if doc_nodes
          .iter()
          .any(|n| matches!(n.diff_status, Some(DiffStatus::Modified)))
        {
          let mut old_tags = tags.clone();
          if let Some(tags_diff) =
            ctx.ctx.diff.as_ref().and_then(|diff_index| {
              let info = diff_index.get_node_diff(
                &doc_nodes[0].origin.specifier,
                doc_nodes[0].get_name(),
                doc_nodes[0].def.to_kind(),
              )?;
              info
                .diff
                .as_ref()?
                .js_doc_changes
                .as_ref()?
                .tags_change
                .as_ref()
            })
          {
            for added in &tags_diff.added {
              match added {
                JsDocTag::Deprecated { .. } => {
                  // Deprecated is not in SymbolCtx tags, skip
                }
                JsDocTag::Experimental => {
                  old_tags.swap_remove(&Tag::Unstable);
                }
                JsDocTag::Tags { tags: tag_values } => {
                  let new_perms: Vec<_> = tag_values
                    .iter()
                    .filter(|t| t.starts_with("allow-"))
                    .collect();
                  if !new_perms.is_empty() {
                    // Remove permissions that were added
                    old_tags.retain(|t| !matches!(t, Tag::Permissions(_)));
                  }
                }
                _ => {}
              }
            }
            for removed in &tags_diff.removed {
              match removed {
                JsDocTag::Experimental => {
                  old_tags.insert(Tag::Unstable);
                }
                JsDocTag::Tags { tags: tag_values } => {
                  let removed_perms: Box<[Box<str>]> = tag_values
                    .iter()
                    .filter(|t| t.starts_with("allow-"))
                    .cloned()
                    .collect();
                  if !removed_perms.is_empty() {
                    old_tags.insert(Tag::Permissions(removed_perms));
                  }
                }
                _ => {}
              }
            }
          }
          Some(old_tags)
        } else {
          None
        };

        let diff_status = compute_combined_diff_status(doc_nodes);

        SymbolCtx {
          tags: super::util::compute_tag_ctx(tags, old_tags),
          kind: doc_nodes[0].kind.into(),
          subtitle: DocBlockSubtitleCtx::new(ctx, &doc_nodes[0]),
          content: SymbolInnerCtx::new(ctx, doc_nodes, name),
          source_href: ctx
            .ctx
            .href_resolver
            .resolve_source(&doc_nodes[0].location),
          deprecated,
          usage,
          diff_status,
        }
      })
      .collect::<Vec<_>>();

    let diff_status = compute_combined_diff_status(doc_nodes);

    // Strip per-symbol diff_status when it matches the group status —
    // if the whole group is Added or Removed, annotating each symbol
    // individually is redundant.
    if diff_status.is_some() {
      for symbol in &mut symbols {
        if symbol.diff_status == diff_status {
          symbol.diff_status = None;
        }
      }
    }

    SymbolGroupCtx {
      name: name.to_string(),
      symbols,
      diff_status,
    }
  }

  /// In diff_only mode, remove tags without a diff annotation from Modified
  /// symbols, but keep non-diffable visual tags (like "new") which serve as
  /// labels rather than attributes.
  pub fn strip_unchanged_tags(&mut self) {
    if matches!(self.diff_status, Some(DiffStatus::Modified)) {
      for symbol in &mut self.symbols {
        symbol.tags.retain(|t| t.diff.is_some());
      }
    }
  }
}

/// Compute a combined diff_status for a group of nodes with the same name.
/// If nodes have a mix of Added and Removed (kind change), return Modified.
pub(crate) fn compute_combined_diff_status(
  nodes: &[DocNodeWithContext],
) -> Option<DiffStatus> {
  let mut has_added = false;
  let mut has_removed = false;
  let mut first_status = None;

  for node in nodes {
    match &node.diff_status {
      Some(DiffStatus::Added) => has_added = true,
      Some(DiffStatus::Removed) => has_removed = true,
      _ => {}
    }
    if first_status.is_none() {
      if let status @ Some(_) = &node.diff_status {
        first_status = status.clone();
      }
    }
  }

  if has_added && has_removed {
    Some(DiffStatus::Modified)
  } else {
    first_status
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct DocBlockClassSubtitleExtendsCtx {
  href: Option<String>,
  symbol: String,
  type_args: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "snake_case")]
#[serde(tag = "kind", content = "value")]
pub enum DocBlockSubtitleCtx {
  Class {
    implements: Option<Vec<String>>,
    extends: Option<DocBlockClassSubtitleExtendsCtx>,
    #[serde(skip_serializing_if = "Option::is_none")]
    is_abstract_change: Option<crate::diff::Change<bool>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    extends_change: Option<crate::diff::Change<Option<Box<str>>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    implements_added: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    implements_removed: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    super_type_params_added: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    super_type_params_removed: Option<Vec<String>>,
  },
  Interface {
    extends: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    extends_added: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    extends_removed: Option<Vec<String>>,
  },
}

impl DocBlockSubtitleCtx {
  pub const TEMPLATE_CLASS: &'static str = "doc_block_subtitle_class";
  pub const TEMPLATE_INTERFACE: &'static str = "doc_block_subtitle_interface";

  fn new(ctx: &RenderContext, doc_node: &DocNodeWithContext) -> Option<Self> {
    match &doc_node.def {
      DocNodeDef::Class { class_def } => {
        let current_type_params = class_def
          .type_params
          .iter()
          .map(|def| def.name.as_str())
          .collect::<HashSet<&str>>();

        let ctx = &ctx.with_current_type_params(current_type_params);

        let class_diff = ctx.ctx.diff.as_ref().and_then(|diff_index| {
          diff_index
            .get_def_diff(
              &doc_node.origin.specifier,
              doc_node.get_name(),
              doc_node.def.to_kind(),
            )
            .and_then(|d| d.as_class())
        });

        let mut class_implements = None;
        let mut class_extends = None;

        if !class_def.implements.is_empty() {
          let impls = class_def
            .implements
            .iter()
            .map(|extend| render_type_def(ctx, extend))
            .collect::<Vec<String>>();

          class_implements = Some(impls);
        }

        if let Some(extends) = class_def.extends.as_ref() {
          class_extends = Some(DocBlockClassSubtitleExtendsCtx {
            href: ctx.lookup_symbol_href(extends),
            symbol: html_escape::encode_text(extends).into_owned(),
            type_args: super::types::type_arguments(
              ctx,
              &class_def.super_type_params,
            ),
          });
        }

        let is_abstract_change =
          class_diff.and_then(|d| d.is_abstract_change.clone());
        let extends_change = class_diff.and_then(|d| d.extends_change.clone());

        let implements_added = class_diff
          .and_then(|d| d.implements_change.as_ref())
          .map(|ic| {
            ic.added
              .iter()
              .map(|t| t.repr.to_string())
              .collect::<Vec<_>>()
          })
          .filter(|v| !v.is_empty());

        let implements_removed = class_diff
          .and_then(|d| d.implements_change.as_ref())
          .map(|ic| {
            ic.removed
              .iter()
              .map(|t| t.repr.to_string())
              .collect::<Vec<_>>()
          })
          .filter(|v| !v.is_empty());

        let super_type_params_added = class_diff
          .and_then(|d| d.super_type_params_change.as_ref())
          .map(|stpc| {
            stpc
              .added
              .iter()
              .map(|t| t.repr.to_string())
              .collect::<Vec<_>>()
          })
          .filter(|v| !v.is_empty());

        let super_type_params_removed = class_diff
          .and_then(|d| d.super_type_params_change.as_ref())
          .map(|stpc| {
            stpc
              .removed
              .iter()
              .map(|t| t.repr.to_string())
              .collect::<Vec<_>>()
          })
          .filter(|v| !v.is_empty());

        Some(DocBlockSubtitleCtx::Class {
          implements: class_implements,
          extends: class_extends,
          is_abstract_change,
          extends_change,
          implements_added,
          implements_removed,
          super_type_params_added,
          super_type_params_removed,
        })
      }
      DocNodeDef::Interface { interface_def } => {
        let iface_diff = ctx.ctx.diff.as_ref().and_then(|diff_index| {
          diff_index
            .get_def_diff(
              &doc_node.origin.specifier,
              doc_node.get_name(),
              doc_node.def.to_kind(),
            )
            .and_then(|d| d.as_interface())
        });

        let extends_added = iface_diff
          .and_then(|d| d.extends_change.as_ref())
          .map(|ec| {
            ec.added
              .iter()
              .map(|t| t.repr.to_string())
              .collect::<Vec<_>>()
          })
          .filter(|v| !v.is_empty());

        let extends_removed = iface_diff
          .and_then(|d| d.extends_change.as_ref())
          .map(|ec| {
            ec.removed
              .iter()
              .map(|t| t.repr.to_string())
              .collect::<Vec<_>>()
          })
          .filter(|v| !v.is_empty());

        let has_extends = !interface_def.extends.is_empty();
        let has_diff = extends_added.is_some() || extends_removed.is_some();

        if !has_extends && !has_diff {
          return None;
        }

        let current_type_params = interface_def
          .type_params
          .iter()
          .map(|def| def.name.as_str())
          .collect::<HashSet<&str>>();
        let ctx = &ctx.with_current_type_params(current_type_params);

        let extends = interface_def
          .extends
          .iter()
          .map(|extend| render_type_def(ctx, extend))
          .collect::<Vec<String>>();

        Some(DocBlockSubtitleCtx::Interface {
          extends,
          extends_added,
          extends_removed,
        })
      }
      _ => None,
    }
  }
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
pub struct SymbolContentCtx {
  pub id: crate::html::util::Id,
  pub docs: Option<String>,
  pub sections: Vec<SectionCtx>,
}

impl SymbolContentCtx {
  pub const TEMPLATE: &'static str = "symbol_content";
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "snake_case", tag = "kind", content = "value")]
pub enum SymbolInnerCtx {
  Function(function::FunctionCtx),
  Other(SymbolContentCtx),
}

impl SymbolInnerCtx {
  fn new(
    ctx: &RenderContext,
    doc_nodes: &[DocNodeWithContext],
    name: &str,
  ) -> Vec<Self> {
    let mut content_parts = Vec::with_capacity(doc_nodes.len());
    let mut functions = vec![];

    for doc_node in doc_nodes {
      let mut sections = vec![];
      let mut docs =
        crate::html::jsdoc::jsdoc_body_to_html(ctx, &doc_node.js_doc, false);

      if !matches!(doc_node.def, DocNodeDef::Function { .. })
        && let Some(examples) =
          crate::html::jsdoc::jsdoc_examples(ctx, &doc_node.js_doc)
      {
        sections.push(examples);
      }

      sections.extend(match doc_node.def {
        DocNodeDef::Function { .. } => {
          functions.push(doc_node);
          continue;
        }

        DocNodeDef::Variable { .. } => {
          variable::render_variable(ctx, doc_node, name)
        }
        DocNodeDef::Class { .. } => class::render_class(ctx, doc_node, name),
        DocNodeDef::Enum { .. } => r#enum::render_enum(ctx, doc_node),
        DocNodeDef::Interface { .. } => {
          interface::render_interface(ctx, doc_node, name)
        }
        DocNodeDef::TypeAlias { .. } => {
          type_alias::render_type_alias(ctx, doc_node, name)
        }

        DocNodeDef::Namespace { .. } => {
          let namespace_nodes = doc_node.namespace_children.as_ref().unwrap();
          let ns_qualifiers = namespace_nodes
            .first()
            .map(|node| node.ns_qualifiers.clone())
            .unwrap_or_else(|| doc_node.sub_qualifier().into());

          let partitions = super::partition::partition_nodes_by_kind(
            ctx.ctx,
            namespace_nodes.iter().flat_map(|node| {
              if let Some(reference_def) = node.reference_def() {
                Box::new(
                  ctx
                    .ctx
                    .resolve_reference(Some(doc_node), &reference_def.target),
                )
                  as Box<dyn Iterator<Item = Cow<DocNodeWithContext>>>
              } else {
                Box::new(std::iter::once(Cow::Borrowed(node))) as _
              }
            }),
            false,
          );

          let ctx = ctx.with_namespace(ns_qualifiers);

          namespace::render_namespace(partitions.into_iter().map(
            |(title, nodes)| {
              let id = ctx.toc.anchorize(&title);

              (
                ctx.clone(),
                Some(crate::html::util::SectionHeaderCtx {
                  title,
                  anchor: AnchorCtx::new(id),
                  href: None,
                  doc: None,
                }),
                nodes,
              )
            },
          ))
        }
        DocNodeDef::ModuleDoc
        | DocNodeDef::Import { .. }
        | DocNodeDef::Reference { .. } => unreachable!(),
      });

      let references = doc_node
        .js_doc
        .tags
        .iter()
        .filter_map(|tag| {
          if let JsDocTag::See { doc } = tag {
            Some(generate_see(ctx, doc))
          } else {
            None
          }
        })
        .collect::<Vec<_>>();

      if !references.is_empty() {
        sections.push(SectionCtx::new(
          ctx,
          "See",
          SectionContentCtx::See(references),
        ));
      }

      if ctx.ctx.diff_only
        && !is_symbol_added(doc_node)
        && !is_symbol_removed(doc_node)
      {
        crate::html::diff::filter_sections_diff_only(&mut sections, &ctx.toc);
      }

      // If there's a doc text change, re-render docs with inline diff
      if let Some(diff_docs) = ctx.ctx.diff.as_ref().and_then(|diff_index| {
        let info = diff_index.get_node_diff(
          &doc_node.origin.specifier,
          doc_node.get_name(),
          doc_node.def.to_kind(),
        )?;
        let doc_change = info
          .diff
          .as_ref()?
          .js_doc_changes
          .as_ref()?
          .doc_change
          .as_ref()?;
        let old_doc = doc_change.old.as_deref().unwrap_or_default();
        let new_doc = doc_node.js_doc.doc.as_deref().unwrap_or_default();
        crate::html::jsdoc::render_docs_with_diff(ctx, old_doc, new_doc)
      }) {
        docs = Some(diff_docs);
      }

      content_parts.push(SymbolInnerCtx::Other(SymbolContentCtx {
        id: crate::html::util::Id::empty(),
        sections,
        docs,
      }));
    }

    if !functions.is_empty() {
      content_parts.push(SymbolInnerCtx::Function(function::FunctionCtx::new(
        ctx, functions,
      )));
    }

    content_parts
  }
}

/// Reconstruct old tags by reversing modifier diffs. Pass `None` for any
/// modifier that the member type doesn't have (e.g. interfaces have no
/// accessibility).
pub(crate) fn compute_old_tags(
  current_tags: &indexmap::IndexSet<Tag>,
  accessibility_change: Option<
    &crate::diff::Change<Option<deno_ast::swc::ast::Accessibility>>,
  >,
  readonly_change: Option<&crate::diff::Change<bool>>,
  abstract_change: Option<&crate::diff::Change<bool>>,
  optional_change: Option<&crate::diff::Change<bool>>,
) -> indexmap::IndexSet<Tag> {
  let mut old_tags = current_tags.clone();

  if let Some(change) = accessibility_change {
    if let Some(new_tag) = Tag::from_accessibility(change.new) {
      old_tags.swap_remove(&new_tag);
    }
    if let Some(old_tag) = Tag::from_accessibility(change.old) {
      old_tags.insert(old_tag);
    }
  }

  if let Some(change) = readonly_change {
    if change.new && !change.old {
      old_tags.swap_remove(&Tag::Readonly);
    } else if !change.new && change.old {
      old_tags.insert(Tag::Readonly);
    }
  }

  if let Some(change) = abstract_change {
    if change.new && !change.old {
      old_tags.swap_remove(&Tag::Abstract);
    } else if !change.new && change.old {
      old_tags.insert(Tag::Abstract);
    }
  }

  if let Some(change) = optional_change {
    if change.new && !change.old {
      old_tags.swap_remove(&Tag::Optional);
    } else if !change.new && change.old {
      old_tags.insert(Tag::Optional);
    }
  }

  old_tags
}

/// Push a removed-property entry (shared between class and interface).
pub(crate) fn push_removed_property_entry(
  ctx: &RenderContext,
  name: &str,
  ts_type: Option<&crate::ts_type::TsTypeDef>,
  location: &crate::Location,
  entries: &mut Vec<crate::html::util::DocEntryCtx>,
) {
  let id = crate::html::util::IdBuilder::new(ctx)
    .kind(crate::html::util::IdKind::Property)
    .name(name)
    .build();

  let ts_type = ts_type
    .map(|t| crate::html::types::render_type_def_colon(ctx, t))
    .unwrap_or_default();

  entries.push(crate::html::util::DocEntryCtx::removed(
    ctx,
    id,
    Some(html_escape::encode_text(name).into_owned()),
    None,
    &ts_type,
    Default::default(),
    None,
    location,
  ));
}

/// Push a removed-method entry (shared between class and interface).
pub(crate) fn push_removed_method_entry(
  ctx: &RenderContext,
  name: &str,
  content: &str,
  location: &crate::Location,
  entries: &mut Vec<crate::html::util::DocEntryCtx>,
) {
  let id = crate::html::util::IdBuilder::new(ctx)
    .kind(crate::html::util::IdKind::Method)
    .name(name)
    .index(0)
    .build();

  entries.push(crate::html::util::DocEntryCtx::removed(
    ctx,
    id,
    Some(html_escape::encode_text(name).into_owned()),
    None,
    content,
    Default::default(),
    None,
    location,
  ));
}

/// Trait for accessing individual index signature diff fields, shared between
/// `IndexSignatureDiff` (class) and `InterfaceIndexSignatureDiff` (interface).
pub(crate) trait IndexSigDiffItem {
  fn index(&self) -> usize;
  fn readonly_change(&self) -> Option<&crate::diff::Change<bool>>;
  fn params_change(&self) -> Option<&crate::diff::ParamsDiff>;
  fn type_change(&self) -> Option<&crate::diff::TsTypeDiff>;
}

impl IndexSigDiffItem for crate::diff::IndexSignatureDiff {
  fn index(&self) -> usize {
    self.index
  }
  fn readonly_change(&self) -> Option<&crate::diff::Change<bool>> {
    self.readonly_change.as_ref()
  }
  fn params_change(&self) -> Option<&crate::diff::ParamsDiff> {
    self.params_change.as_ref()
  }
  fn type_change(&self) -> Option<&crate::diff::TsTypeDiff> {
    self.type_change.as_ref()
  }
}

impl IndexSigDiffItem for crate::diff::InterfaceIndexSignatureDiff {
  fn index(&self) -> usize {
    self.index
  }
  fn readonly_change(&self) -> Option<&crate::diff::Change<bool>> {
    self.readonly_change.as_ref()
  }
  fn params_change(&self) -> Option<&crate::diff::ParamsDiff> {
    self.params_change.as_ref()
  }
  fn type_change(&self) -> Option<&crate::diff::TsTypeDiff> {
    self.type_change.as_ref()
  }
}

/// Shared rendering for index signatures with diff annotations.
/// `diff_status_fn` computes the diff status for a given index signature
/// because the strategy differs between classes and interfaces.
pub(crate) fn render_index_signatures_with_diff<D: IndexSigDiffItem>(
  ctx: &RenderContext,
  index_signatures: &[crate::ts_type::IndexSignatureDef],
  removed: &[crate::ts_type::IndexSignatureDef],
  modified: &[D],
  diff_status_fn: impl Fn(
    usize,
    &crate::ts_type::IndexSignatureDef,
  ) -> Option<DiffStatus>,
) -> Option<SectionCtx> {
  if index_signatures.is_empty() && removed.is_empty() {
    return None;
  }

  let mut items = Vec::with_capacity(index_signatures.len());

  for (i, index_signature) in index_signatures.iter().enumerate() {
    let id = crate::html::util::IdBuilder::new(ctx)
      .kind(crate::html::util::IdKind::IndexSignature)
      .index(i)
      .build();

    let ts_type = index_signature
      .ts_type
      .as_ref()
      .map(|ts_type| crate::html::types::render_type_def_colon(ctx, ts_type))
      .unwrap_or_default();

    let diff_status = diff_status_fn(i, index_signature);

    let (old_readonly, old_params, old_ts_type) =
      if matches!(diff_status, Some(DiffStatus::Modified)) {
        let sig_diff = modified.iter().find(|m| m.index() == i);
        let old_readonly =
          sig_diff.and_then(|sd| sd.readonly_change()).map(|c| c.old);
        let old_params = sig_diff.and_then(|sd| sd.params_change()).map(|pc| {
          function::render_old_index_sig_params(
            ctx,
            &index_signature.params,
            pc,
          )
        });
        let old_ts_type = sig_diff
          .and_then(|sd| sd.type_change())
          .map(|tc| crate::html::types::render_type_def_colon(ctx, &tc.old));
        (old_readonly, old_params, old_ts_type)
      } else {
        (None, None, None)
      };

    items.push(class::IndexSignatureCtx {
      anchor: AnchorCtx { id },
      readonly: index_signature.readonly,
      params: crate::html::parameters::render_params(
        ctx,
        &index_signature.params,
      ),
      ts_type,
      source_href: ctx
        .ctx
        .href_resolver
        .resolve_source(&index_signature.location),
      diff_status,
      old_readonly,
      old_params,
      old_ts_type,
    });
  }

  // Inject removed index signatures
  for removed_sig in removed {
    let id = crate::html::util::IdBuilder::new(ctx)
      .kind(crate::html::util::IdKind::IndexSignature)
      .index(items.len())
      .build();

    let ts_type = removed_sig
      .ts_type
      .as_ref()
      .map(|ts_type| crate::html::types::render_type_def_colon(ctx, ts_type))
      .unwrap_or_default();

    items.push(class::IndexSignatureCtx {
      anchor: AnchorCtx { id },
      readonly: removed_sig.readonly,
      params: crate::html::parameters::render_params(ctx, &removed_sig.params),
      ts_type,
      source_href: ctx.ctx.href_resolver.resolve_source(&removed_sig.location),
      diff_status: Some(DiffStatus::Removed),
      old_readonly: None,
      old_params: None,
      old_ts_type: None,
    });
  }

  Some(SectionCtx::new(
    ctx,
    "Index Signatures",
    SectionContentCtx::IndexSignature(items),
  ))
}

pub(crate) fn render_type_def_sections(
  ctx: &RenderContext,
  name: &str,
  ts_type: &crate::ts_type::TsTypeDef,
  ts_type_change: Option<&crate::diff::TsTypeDiff>,
  id: crate::html::util::Id,
  section_title: &str,
  location: &crate::Location,
) -> Vec<SectionCtx> {
  let mut sections = vec![];

  if let Some(ts_type_literal) = ts_type.type_literal.as_ref() {
    let type_lit_diff = ts_type_change.and_then(|tc| {
      if let Some(old_lit) = tc.old.type_literal.as_ref() {
        crate::diff::InterfaceDiff::diff_type_literal(old_lit, ts_type_literal)
      } else {
        let empty = crate::ts_type::TsTypeLiteralDef::default();
        crate::diff::InterfaceDiff::diff_type_literal(&empty, ts_type_literal)
      }
    });

    if let Some(tc) = ts_type_change
      && tc.old.type_literal.is_none()
    {
      sections.push(SectionCtx::new(
        ctx,
        section_title,
        SectionContentCtx::DocEntry(vec![
          crate::html::util::DocEntryCtx::removed(
            ctx,
            id.clone(),
            None,
            None,
            &render_type_def(ctx, &tc.old),
            Default::default(),
            None,
            location,
          ),
        ]),
      ));
    }

    if let Some(index_signatures) = interface::render_index_signatures(
      ctx,
      &ts_type_literal.index_signatures,
      type_lit_diff
        .as_ref()
        .and_then(|d| d.index_signature_changes.as_ref()),
    ) {
      sections.push(index_signatures);
    }

    if let Some(call_signatures) = interface::render_call_signatures(
      ctx,
      &ts_type_literal.call_signatures,
      type_lit_diff
        .as_ref()
        .and_then(|d| d.call_signature_changes.as_ref()),
    ) {
      sections.push(call_signatures);
    }

    if let Some(properties) = interface::render_properties(
      ctx,
      name,
      &ts_type_literal.properties,
      type_lit_diff
        .as_ref()
        .and_then(|d| d.property_changes.as_ref()),
    ) {
      sections.push(properties);
    }

    if let Some(methods) = interface::render_methods(
      ctx,
      name,
      &ts_type_literal.methods,
      type_lit_diff
        .as_ref()
        .and_then(|d| d.method_changes.as_ref()),
    ) {
      sections.push(methods);
    }
  } else {
    let (diff_status, old_content) = if let Some(tc) = ts_type_change {
      (
        Some(DiffStatus::Modified),
        Some(render_type_def(ctx, &tc.old)),
      )
    } else {
      (None, None)
    };

    sections.push(SectionCtx::new(
      ctx,
      section_title,
      SectionContentCtx::DocEntry(vec![crate::html::util::DocEntryCtx::new(
        ctx,
        id,
        None,
        None,
        &render_type_def(ctx, ts_type),
        Default::default(),
        None,
        location,
        diff_status,
        old_content,
        None,
        None,
      )]),
    ));

    if let Some(old_lit) =
      ts_type_change.and_then(|tc| tc.old.type_literal.as_ref())
    {
      let empty = crate::ts_type::TsTypeLiteralDef::default();
      let type_lit_diff =
        crate::diff::InterfaceDiff::diff_type_literal(old_lit, &empty);

      if let Some(ref diff) = type_lit_diff {
        if let Some(index_sigs) = interface::render_index_signatures(
          ctx,
          &[],
          diff.index_signature_changes.as_ref(),
        ) {
          sections.push(index_sigs);
        }

        if let Some(call_sigs) = interface::render_call_signatures(
          ctx,
          &[],
          diff.call_signature_changes.as_ref(),
        ) {
          sections.push(call_sigs);
        }

        if let Some(props) = interface::render_properties(
          ctx,
          name,
          &[],
          diff.property_changes.as_ref(),
        ) {
          sections.push(props);
        }

        if let Some(methods) = interface::render_methods(
          ctx,
          name,
          &[],
          diff.method_changes.as_ref(),
        ) {
          sections.push(methods);
        }
      }
    }
  }

  sections
}

fn generate_see(ctx: &RenderContext, doc: &str) -> String {
  let doc = if let Some(href) = ctx.lookup_symbol_href(doc) {
    format!("[{doc}]({href})")
  } else {
    doc.to_string()
  };

  crate::html::jsdoc::render_markdown(ctx, &doc, true)
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AllSymbolsEntrypointCtx {
  pub name: String,
  pub href: String,
  pub anchor: AnchorCtx,
  pub module_doc: ModuleDocCtx,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AllSymbolsCtx {
  pub entrypoints: Vec<AllSymbolsEntrypointCtx>,
}

impl AllSymbolsCtx {
  pub const TEMPLATE: &'static str = "all_symbols";

  pub fn new(ctx: &RenderContext) -> Self {
    let mut entrypoints: Vec<AllSymbolsEntrypointCtx> = ctx
      .ctx
      .doc_nodes
      .keys()
      .map(|short_path| {
        let name = short_path.display_name().to_string();
        let id = ctx.toc.anchorize(&name);
        ctx.toc.add_entry(0, &name, &id);

        AllSymbolsEntrypointCtx {
          name,
          href: ctx.ctx.resolve_path(
            ctx.get_current_resolve(),
            UrlResolveKind::File { file: short_path },
          ),
          anchor: AnchorCtx::new(id),
          module_doc: ModuleDocCtx::new(ctx, short_path, true, true),
        }
      })
      .collect();

    if ctx.ctx.diff_only {
      entrypoints.retain(|ep| !ep.module_doc.sections.sections.is_empty());
    }

    Self { entrypoints }
  }
}

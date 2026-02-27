use super::render_context::RenderContext;
use super::util::*;
use crate::html::ShortPath;
use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;
use crate::node::DocNodeDef;
use serde::Deserialize;
use serde::Serialize;
use std::borrow::Cow;
use std::rc::Rc;

lazy_static! {
  static ref JSDOC_LINK_RE: regex::Regex = regex::Regex::new(
    r"(?m)(?:\[(?P<label>[^]]+)])?\{\s*@link(?P<modifier>code|plain)?\s+(?P<value>[^}]+)}"
  )
  .unwrap();
  static ref LINK_RE: regex::Regex =
    regex::Regex::new(r"(^\.{0,2}\/)|(^[A-Za-z]+:\S)").unwrap();
  static ref MODULE_LINK_RE: regex::Regex =
    regex::Regex::new(r"^\[(\S+)\](?:\.(\S+)|\s|)$").unwrap();
}

fn parse_links<'a>(
  md: &'a str,
  ctx: &RenderContext,
  strip: bool,
) -> Cow<'a, str> {
  JSDOC_LINK_RE.replace_all(md, |captures: &regex::Captures| {
    let code = captures
      .name("modifier")
      .map_or("plain", |modifier_match| modifier_match.as_str())
      == "code";
    let value = captures.name("value").unwrap().as_str();
    let label = captures.name("label").map(|x| x.as_str());

    let (link, mut title) = if let Some((link, title)) =
      value.split_once('|').or_else(|| value.split_once(' '))
    {
      (link.trim(), title.trim().to_string())
    } else {
      (value, "".to_string())
    };
    if let Some(label) = label {
      title = label.trim().to_string();
    }

    let link = if let Some(module_link_captures) = MODULE_LINK_RE.captures(link)
    {
      let module_match = module_link_captures.get(1).unwrap();
      let module_link = module_match.as_str();
      let symbol_match = module_link_captures.get(2);

      let mut link = link.to_string();

      let module = ctx.ctx.doc_nodes.iter().find(|(short_path, _)| {
        short_path.path == module_link
          || short_path.display_name() == module_link
      });

      if let Some((short_path, nodes)) = module {
        if let Some(symbol_match) = symbol_match {
          if nodes
            .iter()
            .any(|node| node.get_qualified_name() == symbol_match.as_str())
          {
            link = ctx.ctx.resolve_path(
              ctx.get_current_resolve(),
              UrlResolveKind::Symbol {
                file: short_path,
                symbol: symbol_match.as_str(),
              },
            );
            if title.is_empty() {
              title = format!(
                "{} {}",
                short_path.display_name(),
                symbol_match.as_str()
              );
            }
          }
        } else {
          link = ctx.ctx.resolve_path(
            ctx.get_current_resolve(),
            short_path.as_resolve_kind(),
          );
          if title.is_empty() {
            title = short_path.display_name().to_string();
          }
        }
      } else if let Some((external_link, external_title)) =
        ctx.ctx.href_resolver.resolve_external_jsdoc_module(
          module_link,
          symbol_match.map(|symbol_match| symbol_match.as_str()),
        )
      {
        link = external_link;
        title = external_title;
      }

      link
    } else {
      link.to_string()
    };

    let (title, link) = if let Some(href) = ctx.lookup_symbol_href(&link) {
      let title = if title.is_empty() {
        link
      } else {
        title.to_string()
      };

      (title, href)
    } else {
      let title = if title.is_empty() {
        link.clone()
      } else {
        title.to_string()
      };

      (title, link)
    };

    if strip {
      title
    } else if LINK_RE.is_match(&link) {
      if code {
        format!("[`{title}`]({link})")
      } else {
        format!("[{title}]({link})")
      }
    } else {
      #[allow(clippy::collapsible_if)]
      if code { format!("`{title}`") } else { title }
    }
  })
}

fn split_markdown_title(md: &str) -> (Option<&str>, Option<&str>) {
  let newline = md.find("\n\n").unwrap_or(usize::MAX);
  let codeblock = md.find("```").unwrap_or(usize::MAX);

  let index = newline.min(codeblock).min(md.len());

  match md.split_at(index) {
    ("", body) => (None, Some(body)),
    (title, "") => (None, Some(title)),
    (title, body) => (Some(title), Some(body)),
  }
}

pub struct MarkdownToHTMLOptions {
  pub title_only: bool,
  pub no_toc: bool,
}

pub type MarkdownStripper = Rc<dyn Fn(&str) -> String>;

pub fn strip(render_ctx: &RenderContext, md: &str) -> String {
  let md = parse_links(md, render_ctx, true);

  (render_ctx.ctx.markdown_stripper)(&md)
}

#[cfg(not(feature = "rust"))]
pub type Anchorizer<'a> = &'a js_sys::Function;
#[cfg(feature = "rust")]
pub type Anchorizer =
  std::sync::Arc<dyn Fn(String, u8) -> String + Send + Sync>;

pub type MarkdownRenderer =
  Rc<dyn Fn(&str, bool, Option<ShortPath>, Anchorizer) -> Option<String>>;

fn render_markdown_inner(
  render_ctx: &RenderContext,
  md: &str,
  render_options: &MarkdownToHTMLOptions,
) -> Option<String> {
  let toc = render_ctx.toc.clone();
  let no_toc = render_options.no_toc;

  let anchorizer = move |content: String, level: u8| {
    let mut anchorizer = toc.anchorizer.lock().unwrap();
    let offset = toc.offset.lock().unwrap();

    let anchor = anchorizer.anchorize(&content);

    if !no_toc {
      let mut toc = toc.toc.lock().unwrap();
      toc.push(crate::html::render_context::ToCEntry {
        level: level + *offset,
        content,
        anchor: anchor.clone(),
      });
    }

    anchor
  };

  #[cfg(not(target_arch = "wasm32"))]
  let anchorizer = std::sync::Arc::new(anchorizer);

  #[cfg(target_arch = "wasm32")]
  let anchorizer = wasm_bindgen::prelude::Closure::wrap(
    Box::new(anchorizer) as Box<dyn Fn(String, u8) -> String>
  );
  #[cfg(target_arch = "wasm32")]
  let anchorizer = wasm_bindgen::JsCast::unchecked_ref::<js_sys::Function>(
    anchorizer.as_ref(),
  );

  let md = parse_links(md, render_ctx, false);

  let file = render_ctx.get_current_resolve().get_file().cloned();

  (render_ctx.ctx.markdown_renderer)(
    &md,
    render_options.title_only,
    file,
    anchorizer,
  )
}

pub fn markdown_to_html(
  render_ctx: &RenderContext,
  md: &str,
  render_options: MarkdownToHTMLOptions,
) -> Option<String> {
  let class_name = if render_options.title_only {
    "markdown_summary"
  } else {
    "markdown"
  };

  render_markdown_inner(render_ctx, md, &render_options)
    .map(|html| format!(r#"<div class="{class_name}">{html}</div>"#))
}

pub(crate) fn render_markdown(
  render_ctx: &RenderContext,
  md: &str,
  no_toc: bool,
) -> String {
  markdown_to_html(
    render_ctx,
    md,
    MarkdownToHTMLOptions {
      title_only: false,
      no_toc,
    },
  )
  .unwrap_or_default()
}

pub(crate) fn jsdoc_body_to_html(
  ctx: &RenderContext,
  js_doc: &JsDoc,
  summary: bool,
) -> Option<String> {
  if summary
    && let Some(doc) = js_doc.tags.iter().find_map(|tag| {
      if let JsDocTag::Summary { doc } = tag {
        Some(doc)
      } else {
        None
      }
    })
  {
    markdown_to_html(
      ctx,
      doc,
      MarkdownToHTMLOptions {
        title_only: false,
        no_toc: false,
      },
    )
  } else if let Some(doc) = js_doc.doc.as_deref() {
    markdown_to_html(
      ctx,
      doc,
      MarkdownToHTMLOptions {
        title_only: summary,
        no_toc: false,
      },
    )
  } else {
    None
  }
}

struct MarkdownBlock {
  source: String,
  is_paragraph: bool,
}

/// Parse markdown into structural blocks using comrak's AST. Each top-level
/// node (paragraph, heading, code block, list, etc.) becomes a block with its
/// original source text and whether it's a paragraph (eligible for inline
/// word-level diffing).
#[cfg(feature = "comrak")]
fn parse_markdown_blocks(md: &str) -> Vec<MarkdownBlock> {
  use comrak::nodes::NodeValue;

  let arena = comrak::Arena::new();
  let options = super::comrak::default_options();
  let root = comrak::parse_document(&arena, md, &options);

  let lines: Vec<&str> = md.lines().collect();
  let mut blocks = Vec::new();

  for child in root.children() {
    let data = child.data.borrow();
    let is_paragraph = matches!(data.value, NodeValue::Paragraph);
    let start = data.sourcepos.start.line.saturating_sub(1);
    let end = data.sourcepos.end.line.min(lines.len());
    let source = lines[start..end].join("\n");
    blocks.push(MarkdownBlock {
      source,
      is_paragraph,
    });
  }

  blocks
}

/// A token from inline markdown content, used for word-level diffing.
/// Text nodes are split into word/whitespace tokens; code spans are
/// kept as atomic units so they diff and render correctly.
#[derive(Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
enum InlineToken {
  Text(String),
  Code(String),
}

impl InlineToken {
  fn render(&self) -> String {
    match self {
      InlineToken::Text(s) => html_escape::encode_text(s).into_owned(),
      InlineToken::Code(s) => {
        format!("<code>{}</code>", html_escape::encode_text(s))
      }
    }
  }

  fn is_whitespace(&self) -> bool {
    matches!(self, InlineToken::Text(s) if s.trim().is_empty())
  }
}

/// Tokenize a paragraph's inline content using comrak's AST.
/// Text nodes are split into word/whitespace tokens; code spans
/// are kept as atomic tokens.
#[cfg(feature = "comrak")]
fn tokenize_paragraph(md: &str) -> Vec<InlineToken> {
  use comrak::nodes::NodeValue;

  let arena = comrak::Arena::new();
  let options = super::comrak::default_options();
  let root = comrak::parse_document(&arena, md, &options);

  let mut tokens = Vec::new();

  fn split_text_into_tokens(s: &str, tokens: &mut Vec<InlineToken>) {
    let mut current = String::new();
    let mut in_ws = s.starts_with(|c: char| c.is_whitespace());
    for ch in s.chars() {
      let is_ws = ch.is_whitespace();
      if !current.is_empty() && in_ws != is_ws {
        tokens.push(InlineToken::Text(std::mem::take(&mut current)));
      }
      in_ws = is_ws;
      current.push(ch);
    }
    if !current.is_empty() {
      tokens.push(InlineToken::Text(current));
    }
  }

  fn walk<'a>(
    node: &'a comrak::nodes::AstNode<'a>,
    tokens: &mut Vec<InlineToken>,
  ) {
    let value = node.data.borrow().value.clone();
    match value {
      NodeValue::Text(s) => split_text_into_tokens(&s, tokens),
      NodeValue::Code(c) => {
        tokens.push(InlineToken::Code(c.literal));
      }
      NodeValue::SoftBreak | NodeValue::LineBreak => {
        tokens.push(InlineToken::Text("\n".to_string()));
      }
      _ => {
        for child in node.children() {
          walk(child, tokens);
        }
      }
    }
  }

  if let Some(para) = root.first_child() {
    for child in para.children() {
      walk(child, &mut tokens);
    }
  }

  tokens
}

/// Build inline word-level diff HTML for a modified paragraph. Tokenizes
/// both paragraphs using comrak's AST (so code spans are atomic units),
/// diffs the token sequences, and produces HTML with `diff-inline` spans.
///
/// Small equal segments (≤ 3 non-whitespace tokens) between changes are
/// absorbed into the surrounding change group so that removed/added runs
/// stay contiguous.
///
/// Returns `None` when no equal segments survive absorption (i.e. the text
/// changed entirely), signalling the caller to render as removed + added
/// blocks instead.
#[cfg(feature = "comrak")]
fn render_word_diff_inline(old_text: &str, new_text: &str) -> Option<String> {
  use similar::DiffOp;

  const ABSORB_THRESHOLD: usize = 3;

  let old_tokens = tokenize_paragraph(old_text);
  let new_tokens = tokenize_paragraph(new_text);

  let ops = similar::capture_diff_slices(
    similar::Algorithm::Patience,
    &old_tokens,
    &new_tokens,
  );

  // Build segments from diff ops.
  #[derive(PartialEq)]
  enum SegTag {
    Equal,
    Delete,
    Insert,
  }
  struct Segment {
    tag: SegTag,
    tokens: Vec<InlineToken>,
  }

  let mut segments: Vec<Segment> = Vec::new();
  for op in &ops {
    match *op {
      DiffOp::Equal { old_index, len, .. } => {
        segments.push(Segment {
          tag: SegTag::Equal,
          tokens: old_tokens[old_index..old_index + len].to_vec(),
        });
      }
      DiffOp::Delete {
        old_index, old_len, ..
      } => {
        segments.push(Segment {
          tag: SegTag::Delete,
          tokens: old_tokens[old_index..old_index + old_len].to_vec(),
        });
      }
      DiffOp::Insert {
        new_index, new_len, ..
      } => {
        segments.push(Segment {
          tag: SegTag::Insert,
          tokens: new_tokens[new_index..new_index + new_len].to_vec(),
        });
      }
      DiffOp::Replace {
        old_index,
        old_len,
        new_index,
        new_len,
      } => {
        segments.push(Segment {
          tag: SegTag::Delete,
          tokens: old_tokens[old_index..old_index + old_len].to_vec(),
        });
        segments.push(Segment {
          tag: SegTag::Insert,
          tokens: new_tokens[new_index..new_index + new_len].to_vec(),
        });
      }
    }
  }

  // Check if any equal segment has enough content to survive absorption.
  // If not, the text changed entirely and we should signal the caller to
  // render as removed + added blocks instead.
  let has_surviving_equal = segments.iter().any(|seg| {
    seg.tag == SegTag::Equal
      && seg.tokens.iter().filter(|t| !t.is_whitespace()).count()
        > ABSORB_THRESHOLD
  });
  if !has_surviving_equal {
    return None;
  }

  // Render with absorption of small equal runs between changes.
  let mut html = String::new();
  let mut removed_buf: Vec<InlineToken> = Vec::new();
  let mut added_buf: Vec<InlineToken> = Vec::new();
  let mut in_change = false;

  let flush_changes = |html: &mut String,
                       removed: &mut Vec<InlineToken>,
                       added: &mut Vec<InlineToken>| {
    if !removed.is_empty() {
      html.push_str("<span class=\"diff-inline diff-removed\">");
      for token in removed.drain(..) {
        html.push_str(&token.render());
      }
      html.push_str("</span>");
    }
    if !added.is_empty() {
      html.push_str("<span class=\"diff-inline diff-added\">");
      for token in added.drain(..) {
        html.push_str(&token.render());
      }
      html.push_str("</span>");
    }
  };

  for seg in &segments {
    match seg.tag {
      SegTag::Equal => {
        let word_count =
          seg.tokens.iter().filter(|t| !t.is_whitespace()).count();
        if in_change && word_count <= ABSORB_THRESHOLD {
          removed_buf.extend(seg.tokens.iter().cloned());
          added_buf.extend(seg.tokens.iter().cloned());
        } else {
          flush_changes(&mut html, &mut removed_buf, &mut added_buf);
          in_change = false;
          for token in &seg.tokens {
            html.push_str(&token.render());
          }
        }
      }
      SegTag::Delete => {
        in_change = true;
        removed_buf.extend(seg.tokens.iter().cloned());
      }
      SegTag::Insert => {
        in_change = true;
        added_buf.extend(seg.tokens.iter().cloned());
      }
    }
  }
  flush_changes(&mut html, &mut removed_buf, &mut added_buf);

  Some(html)
}

#[cfg(not(feature = "comrak"))]
pub(crate) fn render_docs_with_diff(
  _ctx: &RenderContext,
  _old_doc: &str,
  _new_doc: &str,
) -> Option<String> {
  // diffing not supported in wasm for now
  None
}

/// Render docs with block-level diff annotations. Parses old and new markdown
/// into structural blocks via comrak's AST, diffs at the block level, and
/// marks each block as unchanged, `diff-added`, `diff-removed`, or
/// `diff-modified` (with inline word-level highlights for paragraphs).
#[cfg(feature = "comrak")]
pub(crate) fn render_docs_with_diff(
  ctx: &RenderContext,
  old_doc: &str,
  new_doc: &str,
) -> Option<String> {
  use similar::DiffOp;
  use similar::TextDiff;

  if old_doc.is_empty() && new_doc.is_empty() {
    return None;
  }

  let render_opts = MarkdownToHTMLOptions {
    title_only: false,
    no_toc: false,
  };
  let render_opts_no_toc = MarkdownToHTMLOptions {
    title_only: false,
    no_toc: true,
  };

  let old_blocks = parse_markdown_blocks(old_doc);
  let new_blocks = parse_markdown_blocks(new_doc);

  let old_sources = old_blocks
    .iter()
    .map(|b| b.source.as_str())
    .collect::<Vec<_>>();
  let new_sources = new_blocks
    .iter()
    .map(|b| b.source.as_str())
    .collect::<Vec<_>>();

  let diff = TextDiff::from_slices(&old_sources, &new_sources);

  let mut inner_html = String::new();

  for op in diff.ops() {
    match *op {
      DiffOp::Equal { old_index, len, .. } => {
        for block in &old_blocks[old_index..old_index + len] {
          if let Some(html) =
            render_markdown_inner(ctx, &block.source, &render_opts)
          {
            inner_html.push_str(&html);
          }
        }
      }
      DiffOp::Delete {
        old_index, old_len, ..
      } => {
        for block in &old_blocks[old_index..old_index + old_len] {
          if let Some(html) =
            render_markdown_inner(ctx, &block.source, &render_opts_no_toc)
          {
            inner_html.push_str(r#"<div class="diff-removed">"#);
            inner_html.push_str(&html);
            inner_html.push_str("</div>");
          }
        }
      }
      DiffOp::Insert {
        new_index, new_len, ..
      } => {
        for block in &new_blocks[new_index..new_index + new_len] {
          if let Some(html) =
            render_markdown_inner(ctx, &block.source, &render_opts)
          {
            inner_html.push_str(r#"<div class="diff-added">"#);
            inner_html.push_str(&html);
            inner_html.push_str("</div>");
          }
        }
      }
      DiffOp::Replace {
        old_index,
        old_len,
        new_index,
        new_len,
      } => {
        let old_slice = &old_blocks[old_index..old_index + old_len];
        let new_slice = &new_blocks[new_index..new_index + new_len];
        let paired = old_slice.len().min(new_slice.len());

        for i in 0..paired {
          if old_slice[i].is_paragraph
            && new_slice[i].is_paragraph
            && let Some(inline) = render_word_diff_inline(
              &old_slice[i].source,
              &new_slice[i].source,
            )
          {
            inner_html.push_str(r#"<div class="diff-modified"><p>"#);
            inner_html.push_str(&inline);
            inner_html.push_str("</p></div>");
            continue;
          }

          if let Some(html) = render_markdown_inner(
            ctx,
            &old_slice[i].source,
            &render_opts_no_toc,
          ) {
            inner_html.push_str(r#"<div class="diff-removed">"#);
            inner_html.push_str(&html);
            inner_html.push_str("</div>");
          }
          if let Some(html) =
            render_markdown_inner(ctx, &new_slice[i].source, &render_opts)
          {
            inner_html.push_str(r#"<div class="diff-added">"#);
            inner_html.push_str(&html);
            inner_html.push_str("</div>");
          }
        }

        for block in &old_slice[paired..] {
          if let Some(html) =
            render_markdown_inner(ctx, &block.source, &render_opts_no_toc)
          {
            inner_html.push_str(r#"<div class="diff-removed">"#);
            inner_html.push_str(&html);
            inner_html.push_str("</div>");
          }
        }

        for block in &new_slice[paired..] {
          if let Some(html) =
            render_markdown_inner(ctx, &block.source, &render_opts)
          {
            inner_html.push_str(r#"<div class="diff-added">"#);
            inner_html.push_str(&html);
            inner_html.push_str("</div>");
          }
        }
      }
    }
  }

  if inner_html.is_empty() {
    return None;
  }

  Some(format!(r#"<div class="markdown">{inner_html}</div>"#))
}

pub(crate) fn jsdoc_examples(
  ctx: &RenderContext,
  js_doc: &JsDoc,
) -> Option<SectionCtx> {
  let mut i = 0;

  let examples = js_doc
    .tags
    .iter()
    .filter_map(|tag| {
      if let JsDocTag::Example { doc } = tag {
        let example = ExampleCtx::new(ctx, doc, i);
        i += 1;
        Some(example)
      } else {
        None
      }
    })
    .collect::<Vec<ExampleCtx>>();

  if !examples.is_empty() {
    Some(SectionCtx::new(
      ctx,
      "Examples",
      SectionContentCtx::Example(examples),
    ))
  } else {
    None
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ExampleCtx {
  pub anchor: AnchorCtx,
  pub title: String,
  pub markdown_title: String,
  markdown_body: String,
}

impl ExampleCtx {
  pub const TEMPLATE: &'static str = "example";

  pub fn new(render_ctx: &RenderContext, example: &str, i: usize) -> Self {
    let id = IdBuilder::new(render_ctx)
      .kind(IdKind::Example)
      .index(i)
      .build();

    let (maybe_title, body) = split_markdown_title(example);
    let title = if let Some(title) = maybe_title {
      title.to_string()
    } else {
      format!("Example {}", i + 1)
    };

    let markdown_title = render_markdown(render_ctx, &title, false);
    let markdown_body =
      render_markdown(render_ctx, body.unwrap_or_default(), true);

    ExampleCtx {
      anchor: AnchorCtx::new(id),
      title,
      markdown_title,
      markdown_body,
    }
  }
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
pub struct ModuleDocCtx {
  pub deprecated: Option<String>,
  pub sections: super::SymbolContentCtx,
}

impl ModuleDocCtx {
  pub const TEMPLATE: &'static str = "module_doc";

  pub fn new(
    render_ctx: &RenderContext,
    short_path: &ShortPath,
    render_symbols: bool,
    summary: bool,
  ) -> Self {
    let module_doc_nodes = render_ctx.ctx.doc_nodes.get(short_path).unwrap();

    let mut sections = Vec::with_capacity(7);

    let (deprecated, html) = if let Some(node) = module_doc_nodes
      .iter()
      .find(|n| matches!(n.def, DocNodeDef::ModuleDoc))
    {
      let deprecated = node.js_doc.tags.iter().find_map(|tag| {
        if let JsDocTag::Deprecated { doc } = tag {
          Some(render_markdown(
            render_ctx,
            doc.as_deref().unwrap_or_default(),
            false,
          ))
        } else {
          None
        }
      });

      if let Some(examples) = jsdoc_examples(render_ctx, &node.js_doc) {
        sections.push(examples);
      }

      let html = jsdoc_body_to_html(render_ctx, &node.js_doc, summary);

      (deprecated, html)
    } else {
      (None, None)
    };

    if render_symbols {
      let partitions_by_kind = super::partition::partition_nodes_by_kind(
        render_ctx.ctx,
        module_doc_nodes.iter().map(Cow::Borrowed),
        true,
      );

      sections.extend(super::namespace::render_namespace(
        partitions_by_kind.into_iter().map(|(title, nodes)| {
          let id = IdBuilder::new(render_ctx)
            .name(short_path.display_name())
            .name(&title)
            .build();

          render_ctx.toc.add_entry(1, &title, &id);

          (
            render_ctx.clone(),
            Some(SectionHeaderCtx {
              title: title.clone(),
              anchor: AnchorCtx::new(id),
              href: None,
              doc: None,
            }),
            nodes,
          )
        }),
      ));
    }

    if render_ctx.ctx.diff_only {
      super::diff::filter_sections_diff_only(&mut sections, &render_ctx.toc);
    }

    Self {
      deprecated,
      sections: super::SymbolContentCtx {
        id: render_ctx.toc.anchorize("module_doc"),
        docs: html,
        sections,
      },
    }
  }
}

#[cfg(test)]
mod test {
  use crate::DocNode;
  use crate::Location;
  use crate::html::GenerateCtx;
  use crate::html::GenerateOptions;
  use crate::html::HrefResolver;
  use crate::html::UsageComposer;
  use crate::html::UsageComposerEntry;
  use crate::html::href_path_resolve;
  use crate::html::jsdoc::parse_links;
  use deno_ast::ModuleSpecifier;
  use indexmap::IndexMap;
  use std::rc::Rc;

  use crate::html::RenderContext;
  use crate::html::UrlResolveKind;
  use crate::html::usage::UsageToMd;
  use crate::interface::InterfaceDef;
  use crate::js_doc::JsDoc;
  use crate::node::DeclarationKind;

  struct EmptyResolver;

  impl HrefResolver for EmptyResolver {
    fn resolve_path(
      &self,
      current: UrlResolveKind,
      target: UrlResolveKind,
    ) -> String {
      href_path_resolve(current, target)
    }

    fn resolve_global_symbol(&self, _symbol: &[String]) -> Option<String> {
      None
    }

    fn resolve_import_href(
      &self,
      _symbol: &[String],
      _src: &str,
    ) -> Option<String> {
      None
    }

    fn resolve_source(&self, _location: &Location) -> Option<String> {
      None
    }

    fn resolve_external_jsdoc_module(
      &self,
      _module: &str,
      _symbol: Option<&str>,
    ) -> Option<(String, String)> {
      None
    }
  }

  impl UsageComposer for EmptyResolver {
    fn is_single_mode(&self) -> bool {
      true
    }

    fn compose(
      &self,
      current_resolve: UrlResolveKind,
      usage_to_md: UsageToMd,
    ) -> IndexMap<UsageComposerEntry, String> {
      current_resolve
        .get_file()
        .map(|current_file| {
          IndexMap::from([(
            UsageComposerEntry {
              name: "".to_string(),
              icon: None,
            },
            usage_to_md(current_file.display_name(), None),
          )])
        })
        .unwrap_or_default()
    }
  }

  #[test]
  fn parse_links_test() {
    let ctx = GenerateCtx::new(
      GenerateOptions {
        package_name: None,
        main_entrypoint: None,
        href_resolver: Rc::new(EmptyResolver),
        usage_composer: Some(Rc::new(EmptyResolver)),
        rewrite_map: None,
        category_docs: None,
        disable_search: false,
        symbol_redirect_map: None,
        default_symbol_map: None,
        markdown_renderer: crate::html::comrak::create_renderer(
          None, None, None,
        ),
        markdown_stripper: Rc::new(crate::html::comrak::strip),
        head_inject: None,
        id_prefix: None,
        diff_only: false,
      },
      Default::default(),
      Default::default(),
      IndexMap::from([
        (
          ModuleSpecifier::parse("file:///a.ts").unwrap(),
          vec![
            DocNode::interface(
              "foo".into(),
              false,
              Location::default(),
              DeclarationKind::Export,
              JsDoc::default(),
              InterfaceDef {
                def_name: None,
                extends: vec![],
                constructors: vec![],
                methods: vec![],
                properties: vec![],
                call_signatures: vec![],
                index_signatures: vec![],
                type_params: Box::new([]),
              },
            ),
            DocNode::interface(
              "bar".into(),
              false,
              Location::default(),
              DeclarationKind::Export,
              JsDoc::default(),
              InterfaceDef {
                def_name: None,
                extends: vec![],
                constructors: vec![],
                methods: vec![],
                properties: vec![],
                call_signatures: vec![],
                index_signatures: vec![],
                type_params: Box::new([]),
              },
            ),
          ],
        ),
        (
          ModuleSpecifier::parse("file:///b.ts").unwrap(),
          vec![DocNode::interface(
            "baz".into(),
            false,
            Location::default(),
            DeclarationKind::Export,
            JsDoc::default(),
            InterfaceDef {
              def_name: None,
              extends: vec![],
              constructors: vec![],
              methods: vec![],
              properties: vec![],
              call_signatures: vec![],
              index_signatures: vec![],
              type_params: Box::new([]),
            },
          )],
        ),
      ]),
      None,
    )
    .unwrap();

    let (a_short_path, nodes) = ctx.doc_nodes.first().unwrap();

    let render_ctx = RenderContext::new(
      &ctx,
      nodes,
      UrlResolveKind::Symbol {
        file: a_short_path,
        symbol: "foo",
      },
    );

    assert_eq!(
      parse_links("foo {@link https://example.com} bar", &render_ctx, false),
      "foo [https://example.com](https://example.com) bar"
    );
    assert_eq!(
      parse_links(
        "foo {@linkcode https://example.com} bar",
        &render_ctx,
        false
      ),
      "foo [`https://example.com`](https://example.com) bar"
    );

    assert_eq!(
      parse_links(
        "foo {@link https://example.com Example} bar",
        &render_ctx,
        false
      ),
      "foo [Example](https://example.com) bar"
    );
    assert_eq!(
      parse_links(
        "foo {@link https://example.com|Example} bar",
        &render_ctx,
        false
      ),
      "foo [Example](https://example.com) bar"
    );
    assert_eq!(
      parse_links(
        "foo [Example]{@link https://example.com} bar",
        &render_ctx,
        false
      ),
      "foo [Example](https://example.com) bar"
    );
    // [label] takes precedence - consistent with the default JSDoc behaviour
    assert_eq!(
      parse_links(
        "foo [Example (pre)]{@link https://example.com|Example (after)} bar",
        &render_ctx,
        false,
      ),
      "foo [Example (pre)](https://example.com) bar"
    );
    assert_eq!(
      parse_links(
        "foo {@linkcode https://example.com Example} bar",
        &render_ctx,
        false,
      ),
      "foo [`Example`](https://example.com) bar"
    );
    assert_eq!(
      parse_links(
        "foo [Example]{@linkcode https://example.com} bar",
        &render_ctx,
        false
      ),
      "foo [`Example`](https://example.com) bar"
    );

    assert_eq!(
      parse_links("foo {@link unknownSymbol} bar", &render_ctx, false),
      "foo unknownSymbol bar"
    );
    assert_eq!(
      parse_links("foo {@linkcode unknownSymbol} bar", &render_ctx, false),
      "foo `unknownSymbol` bar"
    );

    #[cfg(not(target_os = "windows"))]
    {
      assert_eq!(
        parse_links("foo {@link bar} bar", &render_ctx, false),
        "foo [bar](../.././a.ts/~/bar.html) bar"
      );
      assert_eq!(
        parse_links("foo {@linkcode bar} bar", &render_ctx, false),
        "foo [`bar`](../.././a.ts/~/bar.html) bar"
      );

      assert_eq!(
        parse_links("foo {@link [b.ts]} bar", &render_ctx, false),
        "foo [b.ts](../.././b.ts/index.html) bar"
      );
      assert_eq!(
        parse_links("foo {@linkcode [b.ts]} bar", &render_ctx, false),
        "foo [`b.ts`](../.././b.ts/index.html) bar"
      );

      assert_eq!(
        parse_links("foo {@link [b.ts].baz} bar", &render_ctx, false),
        "foo [b.ts baz](../.././b.ts/~/baz.html) bar"
      );
      assert_eq!(
        parse_links("foo {@linkcode [b.ts].baz} bar", &render_ctx, false),
        "foo [`b.ts baz`](../.././b.ts/~/baz.html) bar"
      );
    }
  }
}

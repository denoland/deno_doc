use super::render_context::RenderContext;
use super::util::*;
use crate::html::ShortPath;
use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;
use crate::node::DocNodeDef;
use serde::Serialize;
use std::borrow::Cow;
use std::rc::Rc;

lazy_static! {
  static ref JSDOC_LINK_RE: regex::Regex = regex::Regex::new(
    r"(?m)\{\s*@link(?P<modifier>code|plain)?\s+(?P<value>[^}]+)}"
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

    let (link, mut title) = if let Some((link, title)) =
      value.split_once('|').or_else(|| value.split_once(' '))
    {
      (link.trim(), title.trim().to_string())
    } else {
      (value, "".to_string())
    };

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
      if code {
        format!("`{title}`")
      } else {
        title
      }
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

pub type MarkdownStripper = std::rc::Rc<dyn (Fn(&str) -> String)>;

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
  Rc<dyn (Fn(&str, bool, Option<ShortPath>, Anchorizer) -> Option<String>)>;

pub fn markdown_to_html(
  render_ctx: &RenderContext,
  md: &str,
  render_options: MarkdownToHTMLOptions,
) -> Option<String> {
  let toc = render_ctx.toc.clone();

  let anchorizer = move |content: String, level: u8| {
    let mut anchorizer = toc.anchorizer.lock().unwrap();
    let offset = toc.offset.lock().unwrap();

    let anchor = anchorizer.anchorize(&content);

    if !render_options.no_toc {
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
  if let Some(doc) = js_doc.doc.as_deref() {
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

#[derive(Debug, Serialize, Clone)]
pub struct ExampleCtx {
  pub anchor: AnchorCtx,
  pub id: Id,
  pub title: String,
  pub markdown_title: String,
  markdown_body: String,
}

impl ExampleCtx {
  pub const TEMPLATE: &'static str = "example";

  pub fn new(render_ctx: &RenderContext, example: &str, i: usize) -> Self {
    let id = IdBuilder::new(render_ctx.ctx)
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
      anchor: AnchorCtx { id: id.clone() },
      id,
      title,
      markdown_title,
      markdown_body,
    }
  }
}

#[derive(Debug, Serialize, Clone, Default)]
pub struct ModuleDocCtx {
  pub deprecated: Option<String>,
  pub sections: super::SymbolContentCtx,
}

impl ModuleDocCtx {
  pub const TEMPLATE: &'static str = "module_doc";

  pub fn new(render_ctx: &RenderContext, short_path: &ShortPath) -> Self {
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

      let html = jsdoc_body_to_html(render_ctx, &node.js_doc, false);

      (deprecated, html)
    } else {
      (None, None)
    };

    if !short_path.is_main {
      let partitions_by_kind = super::partition::partition_nodes_by_kind(
        render_ctx.ctx,
        module_doc_nodes.iter().map(Cow::Borrowed),
        true,
      );

      sections.extend(super::namespace::render_namespace(
        partitions_by_kind.into_iter().map(|(title, nodes)| {
          (
            render_ctx.clone(),
            Some(SectionHeaderCtx {
              title: title.clone(),
              anchor: AnchorCtx {
                id: super::util::Id::new(title),
              },
              href: None,
              doc: None,
            }),
            nodes,
          )
        }),
      ));
    }

    Self {
      deprecated,
      sections: super::SymbolContentCtx {
        id: Id::new("module_doc"),
        docs: html,
        sections,
      },
    }
  }
}

#[cfg(test)]
mod test {
  use crate::html::href_path_resolve;
  use crate::html::jsdoc::parse_links;
  use crate::html::GenerateCtx;
  use crate::html::GenerateOptions;
  use crate::html::HrefResolver;
  use crate::html::UsageComposer;
  use crate::html::UsageComposerEntry;
  use crate::DocNode;
  use crate::Location;
  use deno_ast::ModuleSpecifier;
  use indexmap::IndexMap;
  use std::rc::Rc;

  use crate::html::usage::UsageToMd;
  use crate::html::RenderContext;
  use crate::html::UrlResolveKind;
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
        usage_composer: Rc::new(EmptyResolver),
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
        "foo {@linkcode https://example.com Example} bar",
        &render_ctx,
        false,
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
        "foo [bar](../../.././/a.ts/~/bar.html) bar"
      );
      assert_eq!(
        parse_links("foo {@linkcode bar} bar", &render_ctx, false),
        "foo [`bar`](../../.././/a.ts/~/bar.html) bar"
      );

      assert_eq!(
        parse_links("foo {@link [b.ts]} bar", &render_ctx, false),
        "foo [b.ts](../../.././/b.ts/index.html) bar"
      );
      assert_eq!(
        parse_links("foo {@linkcode [b.ts]} bar", &render_ctx, false),
        "foo [`b.ts`](../../.././/b.ts/index.html) bar"
      );

      assert_eq!(
        parse_links("foo {@link [b.ts].baz} bar", &render_ctx, false),
        "foo [b.ts baz](../../.././/b.ts/~/baz.html) bar"
      );
      assert_eq!(
        parse_links("foo {@linkcode [b.ts].baz} bar", &render_ctx, false),
        "foo [`b.ts baz`](../../.././/b.ts/~/baz.html) bar"
      );
    }
  }
}

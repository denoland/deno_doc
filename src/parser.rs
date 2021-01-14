// Copyright 2020-2021 the Deno authors. All rights reserved. MIT license.
use crate::swc_util::AstParser;
use swc_common::comments::CommentKind;
use swc_common::Span;
use swc_ecmascript::ast::Decl;
use swc_ecmascript::ast::DefaultDecl;
use swc_ecmascript::ast::ExportSpecifier;
use swc_ecmascript::ast::ModuleDecl;
use swc_ecmascript::ast::Stmt;
use swc_ecmascript::parser::Syntax;

use crate::namespace::NamespaceDef;
use crate::node;
use crate::node::DocNode;
use crate::node::ModuleDoc;
use crate::swc_util;
use crate::ImportDef;
use crate::Location;
use futures::Future;
use futures::FutureExt;
use regex::Regex;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::pin::Pin;

#[derive(Debug)]
pub enum DocError {
  Resolve(String),
  Io(std::io::Error),
  Parse(swc_util::SwcDiagnosticBuffer),
}

impl Error for DocError {}

impl fmt::Display for DocError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let m = match self {
      Self::Resolve(s) => s.to_string(),
      Self::Io(err) => err.to_string(),
      Self::Parse(err) => err.to_string(),
    };
    f.pad(&m)
  }
}

impl From<swc_util::SwcDiagnosticBuffer> for DocError {
  fn from(error: swc_util::SwcDiagnosticBuffer) -> DocError {
    DocError::Parse(error)
  }
}

pub trait DocFileLoader {
  fn resolve(
    &self,
    specifier: &str,
    referrer: &str,
  ) -> Result<String, DocError>;

  fn load_source_code(
    &self,
    specifier: &str,
  ) -> Pin<Box<dyn Future<Output = Result<String, DocError>>>>;
}

pub struct DocParser {
  pub ast_parser: AstParser,
  pub loader: Box<dyn DocFileLoader>,
  pub private: bool,
}

impl DocParser {
  pub fn new(loader: Box<dyn DocFileLoader>, private: bool) -> Self {
    DocParser {
      loader,
      ast_parser: AstParser::default(),
      private,
    }
  }

  pub fn parse_module(
    &self,
    file_name: &str,
    syntax: Syntax,
    source_code: &str,
  ) -> Result<ModuleDoc, DocError> {
    let parse_result =
      self.ast_parser.parse_module(file_name, syntax, source_code);
    let module = parse_result?;
    let mut doc_entries =
      self.get_doc_nodes_for_module_body(module.body.clone());
    let import_doc_entries =
      self.get_doc_nodes_for_module_imports(module.body.clone(), file_name)?;
    doc_entries.extend(import_doc_entries);
    let reexports = self.get_reexports_for_module_body(module.body);
    let module_doc = ModuleDoc {
      definitions: doc_entries,
      reexports,
    };
    Ok(module_doc)
  }

  pub async fn parse(
    &self,
    file_name: &str,
    syntax: Syntax,
  ) -> Result<Vec<DocNode>, DocError> {
    let source_code = self.loader.load_source_code(file_name).await?;

    self.parse_source(file_name, syntax, source_code.as_str())
  }

  pub fn parse_source(
    &self,
    file_name: &str,
    syntax: Syntax,
    source_code: &str,
  ) -> Result<Vec<DocNode>, DocError> {
    let module_doc = self.parse_module(file_name, syntax, &source_code)?;
    Ok(module_doc.definitions)
  }

  async fn flatten_reexports(
    &self,
    reexports: &[node::Reexport],
    referrer: &str,
    syntax: Syntax,
  ) -> Result<Vec<DocNode>, DocError> {
    let mut by_src: HashMap<String, Vec<node::Reexport>> = HashMap::new();

    let mut processed_reexports: Vec<DocNode> = vec![];

    for reexport in reexports {
      if by_src.get(&reexport.src).is_none() {
        by_src.insert(reexport.src.to_string(), vec![]);
      }

      let bucket = by_src.get_mut(&reexport.src).unwrap();
      bucket.push(reexport.clone());
    }

    for specifier in by_src.keys() {
      let resolved_specifier = self.loader.resolve(specifier, referrer)?;
      let doc_nodes = self
        .parse_with_reexports(&resolved_specifier, syntax)
        .await?;
      let reexports_for_specifier = by_src.get(specifier).unwrap();

      for reexport in reexports_for_specifier {
        match &reexport.kind {
          node::ReexportKind::All => {
            processed_reexports.extend(doc_nodes.clone())
          }
          node::ReexportKind::Namespace(ns_name) => {
            let ns_def = NamespaceDef {
              elements: doc_nodes.clone(),
            };
            let ns_doc_node = DocNode::namespace(
              ns_name.to_string(),
              Location {
                filename: specifier.to_string(),
                line: 1,
                col: 0,
              },
              None,
              ns_def,
            );
            processed_reexports.push(ns_doc_node);
          }
          node::ReexportKind::Named(ident, maybe_alias) => {
            // Try to find reexport.
            // NOTE: the reexport might actually be reexport from another
            // module; for now we're skipping nested reexports.
            let doc_nodes = doc_nodes
              .iter()
              .filter(|node| &node.name == ident)
              .collect::<Vec<_>>();

            for doc_node in doc_nodes {
              let doc_node = doc_node.clone();
              let doc_node = if let Some(alias) = maybe_alias {
                DocNode {
                  name: alias.to_string(),
                  ..doc_node
                }
              } else {
                doc_node
              };

              processed_reexports.push(doc_node);
            }
          }
          node::ReexportKind::Default => {
            // TODO: handle default export from child module
          }
        }
      }
    }

    Ok(processed_reexports)
  }

  pub fn parse_with_reexports<'a>(
    &'a self,
    file_name: &'a str,
    syntax: Syntax,
  ) -> Pin<Box<dyn Future<Output = Result<Vec<DocNode>, DocError>> + 'a>> {
    async move {
      let source_code = self.loader.load_source_code(file_name).await?;

      let module_doc = self.parse_module(file_name, syntax, &source_code)?;

      let flattened_docs = if !module_doc.reexports.is_empty() {
        let mut flattenned_reexports = self
          .flatten_reexports(&module_doc.reexports, file_name, syntax)
          .await?;
        flattenned_reexports.extend(module_doc.definitions);
        flattenned_reexports
      } else {
        module_doc.definitions
      };

      Ok(flattened_docs)
    }
    .boxed_local()
  }

  fn get_doc_nodes_for_module_imports(
    &self,
    module_body: Vec<swc_ecmascript::ast::ModuleItem>,
    referrer: &str,
  ) -> Result<Vec<DocNode>, DocError> {
    let mut imports = vec![];

    for node in module_body.iter() {
      if let swc_ecmascript::ast::ModuleItem::ModuleDecl(module_decl) = node {
        if let ModuleDecl::Import(import_decl) = module_decl {
          let (js_doc, location) = self.details_for_span(import_decl.span);
          for specifier in &import_decl.specifiers {
            use swc_ecmascript::ast::ImportSpecifier::*;

            let (name, maybe_imported_name, src) = match specifier {
              Named(named_specifier) => (
                named_specifier.local.sym.to_string(),
                named_specifier
                  .imported
                  .as_ref()
                  .map(|ident| ident.sym.to_string())
                  .or_else(|| Some(named_specifier.local.sym.to_string())),
                import_decl.src.value.to_string(),
              ),
              Default(default_specifier) => (
                default_specifier.local.sym.to_string(),
                Some("default".to_string()),
                import_decl.src.value.to_string(),
              ),
              Namespace(namespace_specifier) => (
                namespace_specifier.local.sym.to_string(),
                None,
                import_decl.src.value.to_string(),
              ),
            };

            let resolved_specifier = self.loader.resolve(&src, referrer)?;
            let import_def = ImportDef {
              src: resolved_specifier,
              imported: maybe_imported_name,
            };

            let doc_node = DocNode::import(
              name,
              location.clone(),
              js_doc.clone(),
              import_def,
            );

            imports.push(doc_node);
          }
        }
      }
    }

    Ok(imports)
  }

  pub fn get_doc_nodes_for_module_exports(
    &self,
    module_decl: &ModuleDecl,
  ) -> Vec<DocNode> {
    match module_decl {
      ModuleDecl::ExportDecl(export_decl) => {
        vec![super::module::get_doc_node_for_export_decl(
          self,
          export_decl,
        )]
      }
      ModuleDecl::ExportDefaultDecl(export_default_decl) => {
        let (js_doc, location) =
          self.details_for_span(export_default_decl.span);
        let name = "default".to_string();

        let doc_node = match &export_default_decl.decl {
          DefaultDecl::Class(class_expr) => {
            let class_def =
              crate::class::class_to_class_def(self, &class_expr.class);
            DocNode::class(name, location, js_doc, class_def)
          }
          DefaultDecl::Fn(fn_expr) => {
            let function_def = crate::function::function_to_function_def(
              self,
              &fn_expr.function,
            );
            DocNode::function(name, location, js_doc, function_def)
          }
          DefaultDecl::TsInterfaceDecl(interface_decl) => {
            let (_, interface_def) =
              crate::interface::get_doc_for_ts_interface_decl(
                self,
                interface_decl,
              );
            DocNode::interface(name, location, js_doc, interface_def)
          }
        };

        vec![doc_node]
      }
      ModuleDecl::ExportDefaultExpr(_export_default_expr) => vec![],
      _ => vec![],
    }
  }

  fn details_for_span(&self, span: Span) -> (Option<String>, Location) {
    let js_doc = self.js_doc_for_span(span);
    let location = self.ast_parser.get_span_location(span).into();
    (js_doc, location)
  }

  pub fn get_doc_node_for_decl(&self, decl: &Decl) -> Option<DocNode> {
    match decl {
      Decl::Class(class_decl) => {
        let (name, class_def) =
          super::class::get_doc_for_class_decl(self, class_decl);
        let (js_doc, location) = self.details_for_span(class_decl.class.span);
        Some(DocNode::class(name, location, js_doc, class_def))
      }
      Decl::Fn(fn_decl) => {
        let (name, function_def) =
          super::function::get_doc_for_fn_decl(self, fn_decl);
        let (js_doc, location) = self.details_for_span(fn_decl.function.span);
        Some(DocNode::function(name, location, js_doc, function_def))
      }
      Decl::Var(var_decl) => {
        let (name, var_def) = super::variable::get_doc_for_var_decl(var_decl);
        let (js_doc, location) = self.details_for_span(var_decl.span);
        Some(DocNode::variable(name, location, js_doc, var_def))
      }
      Decl::TsInterface(ts_interface_decl) => {
        let (name, interface_def) =
          super::interface::get_doc_for_ts_interface_decl(
            self,
            ts_interface_decl,
          );
        let (js_doc, location) = self.details_for_span(ts_interface_decl.span);
        Some(DocNode::interface(name, location, js_doc, interface_def))
      }
      Decl::TsTypeAlias(ts_type_alias) => {
        let (name, type_alias_def) =
          super::type_alias::get_doc_for_ts_type_alias_decl(
            self,
            ts_type_alias,
          );
        let (js_doc, location) = self.details_for_span(ts_type_alias.span);
        Some(DocNode::type_alias(name, location, js_doc, type_alias_def))
      }
      Decl::TsEnum(ts_enum) => {
        let (name, enum_def) =
          super::r#enum::get_doc_for_ts_enum_decl(self, ts_enum);
        let (js_doc, location) = self.details_for_span(ts_enum.span);
        Some(DocNode::r#enum(name, location, js_doc, enum_def))
      }
      Decl::TsModule(ts_module) => {
        let (name, namespace_def) =
          super::namespace::get_doc_for_ts_module(self, ts_module);
        let (js_doc, location) = self.details_for_span(ts_module.span);
        Some(DocNode::namespace(name, location, js_doc, namespace_def))
      }
    }
  }

  pub fn get_reexports_for_module_body(
    &self,
    module_body: Vec<swc_ecmascript::ast::ModuleItem>,
  ) -> Vec<node::Reexport> {
    use swc_ecmascript::ast::ExportSpecifier::*;

    let mut reexports: Vec<node::Reexport> = vec![];

    for node in module_body.iter() {
      if let swc_ecmascript::ast::ModuleItem::ModuleDecl(module_decl) = node {
        let r = match module_decl {
          ModuleDecl::ExportNamed(named_export) => {
            if let Some(src) = &named_export.src {
              let src_str = src.value.to_string();
              named_export
                .specifiers
                .iter()
                .map(|export_specifier| match export_specifier {
                  Namespace(ns_export) => node::Reexport {
                    kind: node::ReexportKind::Namespace(
                      ns_export.name.sym.to_string(),
                    ),
                    src: src_str.to_string(),
                  },
                  Default(_) => node::Reexport {
                    kind: node::ReexportKind::Default,
                    src: src_str.to_string(),
                  },
                  Named(named_export) => {
                    let ident = named_export.orig.sym.to_string();
                    let maybe_alias =
                      named_export.exported.as_ref().map(|e| e.sym.to_string());
                    let kind = node::ReexportKind::Named(ident, maybe_alias);
                    node::Reexport {
                      kind,
                      src: src_str.to_string(),
                    }
                  }
                })
                .collect::<Vec<node::Reexport>>()
            } else {
              vec![]
            }
          }
          ModuleDecl::ExportAll(export_all) => {
            let reexport = node::Reexport {
              kind: node::ReexportKind::All,
              src: export_all.src.value.to_string(),
            };
            vec![reexport]
          }
          _ => vec![],
        };

        reexports.extend(r);
      }
    }

    reexports
  }

  pub fn get_doc_nodes_for_module_body(
    &self,
    module_body: Vec<swc_ecmascript::ast::ModuleItem>,
  ) -> Vec<DocNode> {
    let mut unexported_doc_map: HashMap<String, DocNode> = HashMap::new();
    let mut doc_entries: Vec<DocNode> = vec![];

    for node in module_body.iter() {
      if let swc_ecmascript::ast::ModuleItem::Stmt(stmt) = node {
        if let Stmt::Decl(decl) = stmt {
          if let Some(doc_node) = self.get_doc_node_for_decl(decl) {
            let is_declared = self.get_declare_for_decl(decl);
            if is_declared || self.private {
              doc_entries.push(doc_node);
            } else {
              unexported_doc_map.insert(doc_node.name.clone(), doc_node);
            }
          }
        }
      }
    }

    for node in module_body.iter() {
      if let swc_ecmascript::ast::ModuleItem::ModuleDecl(module_decl) = node {
        doc_entries.extend(self.get_doc_nodes_for_module_exports(module_decl));

        if let ModuleDecl::ExportNamed(export_named) = module_decl {
          for specifier in &export_named.specifiers {
            match specifier {
              ExportSpecifier::Named(named_specifier) => {
                if let Some(doc_node) = unexported_doc_map
                  .get_mut(&named_specifier.orig.sym.to_string())
                {
                  if let Some(exported_ident) = &named_specifier.exported {
                    doc_node.name = exported_ident.sym.to_string();
                  }
                  doc_entries.push(doc_node.clone());
                }
              }
              // TODO(zhmushan)
              ExportSpecifier::Default(_default_specifier) => {}
              ExportSpecifier::Namespace(_namespace_specifier) => {}
            }
          }
        }
      }
    }

    doc_entries
  }

  pub fn js_doc_for_span(&self, span: Span) -> Option<String> {
    let comments = self.ast_parser.get_span_comments(span);
    let js_doc_comment = comments.iter().rev().find(|comment| {
      comment.kind == CommentKind::Block && comment.text.starts_with('*')
    })?;

    let mut margin_pat = String::from("");
    if let Some(margin) = self.ast_parser.source_map.span_to_margin(span) {
      for _ in 0..margin {
        margin_pat.push(' ');
      }
    }

    let js_doc_re = Regex::new(r#" ?\* ?"#).unwrap();
    let txt = js_doc_comment
      .text
      .split('\n')
      .map(|line| js_doc_re.replace(line, "").to_string())
      .map(|line| {
        if line.starts_with(&margin_pat) {
          line[margin_pat.len()..].to_string()
        } else {
          line
        }
      })
      .collect::<Vec<String>>()
      .join("\n");

    let txt = txt.trim_start().trim_end().to_string();

    Some(txt)
  }

  fn get_declare_for_decl(&self, decl: &Decl) -> bool {
    match decl {
      Decl::Class(class_decl) => class_decl.declare,
      Decl::Fn(fn_decl) => fn_decl.declare,
      Decl::TsEnum(ts_enum_decl) => ts_enum_decl.declare,
      Decl::TsInterface(ts_interface_decl) => ts_interface_decl.declare,
      Decl::TsModule(ts_module_decl) => ts_module_decl.declare,
      Decl::TsTypeAlias(ts_type_alias_decl) => ts_type_alias_decl.declare,
      Decl::Var(var_decl) => var_decl.declare,
    }
  }
}

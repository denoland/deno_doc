// Copyright 2020-2023 the Deno authors. All rights reserved. MIT license.

use crate::js_doc::JsDoc;
use crate::node::DeclarationKind;
use crate::node::DocNode;
use crate::node::DocNodeDef;
use crate::node::NamespaceDef;
use crate::ts_type::TsTypeDef;
use crate::util::swc::get_text_info_location;
use crate::util::swc::has_ignorable_js_doc_tag;
use crate::util::symbol::symbol_has_ignorable_js_doc_tag;
use crate::variable::VariableDef;
use crate::Location;

use deno_ast::diagnostics::Diagnostic;
use deno_ast::diagnostics::DiagnosticLevel;
use deno_ast::diagnostics::DiagnosticLocation;
use deno_ast::diagnostics::DiagnosticSnippet;
use deno_ast::diagnostics::DiagnosticSnippetHighlight;
use deno_ast::diagnostics::DiagnosticSnippetHighlightStyle;
use deno_ast::diagnostics::DiagnosticSourcePos;
use deno_ast::diagnostics::DiagnosticSourceRange;
use deno_ast::swc::ast::Accessibility;
use deno_ast::ModuleSpecifier;
use deno_ast::SourceRange;
use deno_ast::SourceTextInfo;
use deno_graph::symbols::ModuleInfoRef;
use deno_graph::symbols::RootSymbol;
use deno_graph::symbols::Symbol;
use deno_graph::symbols::UniqueSymbolId;

use std::borrow::Cow;
use std::collections::HashSet;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum DocDiagnosticKind {
  MissingJsDoc,
  MissingExplicitType,
  MissingReturnType,
  PrivateTypeRef(Box<PrivateTypeRefDiagnostic>),
}

#[derive(Debug, Clone)]
pub struct PrivateTypeRefDiagnostic {
  pub name: String,
  pub reference: String,
  pub reference_text_info: SourceTextInfo,
  /// The location of the reference.
  pub reference_location: Location,
}

#[derive(Clone)]
pub struct DocDiagnostic {
  pub location: Location,
  pub kind: DocDiagnosticKind,
  pub text_info: SourceTextInfo,
}

impl std::fmt::Debug for DocDiagnostic {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    // exclude text_info
    f.debug_struct("DocDiagnostic")
      .field("location", &self.location)
      .field("kind", &self.kind)
      .field("text_info", &"<omitted>")
      .finish()
  }
}

impl Diagnostic for DocDiagnostic {
  fn level(&self) -> DiagnosticLevel {
    DiagnosticLevel::Error
  }

  fn code(&self) -> Cow<'_, str> {
    Cow::Borrowed(match self.kind {
      DocDiagnosticKind::MissingJsDoc => "missing-jsdoc",
      DocDiagnosticKind::MissingExplicitType => "missing-explicit-type",
      DocDiagnosticKind::MissingReturnType => "missing-return-type",
      DocDiagnosticKind::PrivateTypeRef { .. } => "private-type-ref",
    })
  }

  fn message(&self) -> Cow<'_, str> {
    match &self.kind {
      DocDiagnosticKind::MissingJsDoc => {
        Cow::Borrowed("exported symbol is missing JSDoc documentation")
      }
      DocDiagnosticKind::MissingExplicitType => {
        Cow::Borrowed("exported symbol is missing an explicit type annotation")
      }
      DocDiagnosticKind::MissingReturnType => Cow::Borrowed(
        "exported function is missing an explicit return type annotation",
      ),
      DocDiagnosticKind::PrivateTypeRef(diagnostic) => Cow::Owned(format!(
        "public type '{}' references private type '{}'",
        diagnostic.name, diagnostic.reference,
      )),
    }
  }

  fn location(&self) -> DiagnosticLocation {
    let specifier = ModuleSpecifier::parse(&self.location.filename).unwrap();
    DiagnosticLocation::ModulePosition {
      specifier: Cow::Owned(specifier),
      source_pos: DiagnosticSourcePos::ByteIndex(self.location.byte_index),
      text_info: Cow::Borrowed(&self.text_info),
    }
  }

  fn snippet(&self) -> Option<DiagnosticSnippet<'_>> {
    let start_byte_index = self.location.byte_index;
    let start_char_len = &self.text_info.text()[start_byte_index..]
      .chars()
      .next()
      .map(|ch| ch.len_utf8())
      .unwrap_or(1);
    Some(DiagnosticSnippet {
      source: Cow::Borrowed(&self.text_info),
      highlights: vec![DiagnosticSnippetHighlight {
        style: DiagnosticSnippetHighlightStyle::Error,
        range: DiagnosticSourceRange {
          start: DiagnosticSourcePos::ByteIndex(start_byte_index),
          end: DiagnosticSourcePos::ByteIndex(
            start_byte_index + start_char_len,
          ),
        },
        description: None,
      }],
    })
  }

  fn hint(&self) -> Option<Cow<'_, str>> {
    match &self.kind {
      DocDiagnosticKind::PrivateTypeRef { .. } => Some(Cow::Borrowed(
        "make the referenced type public or remove the reference",
      )),
      _ => None,
    }
  }
  fn snippet_fixed(&self) -> Option<DiagnosticSnippet<'_>> {
    match &self.kind {
      DocDiagnosticKind::PrivateTypeRef(diagnostic) => {
        Some(DiagnosticSnippet {
          source: Cow::Borrowed(&diagnostic.reference_text_info),
          highlights: vec![DiagnosticSnippetHighlight {
            style: DiagnosticSnippetHighlightStyle::Hint,
            range: DiagnosticSourceRange {
              start: DiagnosticSourcePos::ByteIndex(
                diagnostic.reference_location.byte_index,
              ),
              end: DiagnosticSourcePos::ByteIndex(
                diagnostic.reference_location.byte_index + 1,
              ),
            },
            description: Some(Cow::Borrowed("this is the referenced type")),
          }],
        })
      }
      _ => None,
    }
  }

  fn info(&self) -> std::borrow::Cow<'_, [std::borrow::Cow<'_, str>]> {
    match &self.kind {
      DocDiagnosticKind::MissingJsDoc => Cow::Borrowed(&[]),
      DocDiagnosticKind::MissingExplicitType => Cow::Borrowed(&[]),
      DocDiagnosticKind::MissingReturnType => Cow::Borrowed(&[]),
      DocDiagnosticKind::PrivateTypeRef { .. } => {
        Cow::Borrowed(&[Cow::Borrowed(
          "to ensure documentation is complete all types that are exposed in the public API must be public",
        )])
      }
    }
  }

  fn docs_url(&self) -> Option<Cow<'_, str>> {
    None
  }
}

pub struct DiagnosticsCollector<'a> {
  root_symbol: Rc<RootSymbol<'a>>,
  seen_private_types_in_public: HashSet<(UniqueSymbolId, UniqueSymbolId)>,
  seen_jsdoc_missing: HashSet<Location>,
  seen_missing_type_refs: HashSet<Location>,
  diagnostics: Vec<DocDiagnostic>,
}

impl<'a> DiagnosticsCollector<'a> {
  pub fn new(root_symbol: Rc<RootSymbol<'a>>) -> Self {
    Self {
      root_symbol,
      seen_private_types_in_public: Default::default(),
      seen_jsdoc_missing: Default::default(),
      seen_missing_type_refs: Default::default(),
      diagnostics: Default::default(),
    }
  }

  pub fn add_private_type_in_public(
    &mut self,
    decl_module: ModuleInfoRef,
    decl_name: &str,
    decl_range: SourceRange,
    doc_symbol_id: UniqueSymbolId,
    referenced_module: ModuleInfoRef,
    referenced_symbol: &Symbol,
  ) {
    if !self.seen_private_types_in_public.insert((
      doc_symbol_id,
      UniqueSymbolId::new(
        referenced_module.module_id(),
        referenced_symbol.symbol_id(),
      ),
    )) {
      return;
    }
    if symbol_has_ignorable_js_doc_tag(referenced_module, referenced_symbol) {
      return; // ignore
    }
    let Some(reference) =
      referenced_module.fully_qualified_symbol_name(referenced_symbol)
    else {
      return;
    };

    self.diagnostics.push(DocDiagnostic {
      location: get_text_info_location(
        decl_module.specifier().as_str(),
        decl_module.text_info(),
        decl_range.start,
      ),
      text_info: decl_module.text_info().clone(),
      kind: DocDiagnosticKind::PrivateTypeRef(Box::new(
        PrivateTypeRefDiagnostic {
          name: decl_name.to_string(),
          reference: reference.to_string(),
          reference_text_info: referenced_module.text_info().clone(),
          reference_location: referenced_symbol
            .decls()
            .iter()
            .next()
            .map(|d| {
              get_text_info_location(
                referenced_module.specifier().as_str(),
                referenced_module.text_info(),
                d.range.start,
              )
            })
            // should never happen, but just in case
            .unwrap_or_else(|| Location {
              filename: referenced_module
                .specifier()
                .to_string()
                .into_boxed_str(),
              line: 1,
              col: 0,
              byte_index: 0,
            }),
        },
      )),
    })
  }

  pub fn take_diagnostics(&mut self) -> Vec<DocDiagnostic> {
    let inner = std::mem::take(&mut self.diagnostics);
    *self = Self::new(self.root_symbol.clone()); // reset
    inner
  }

  pub fn analyze_doc_nodes(&mut self, doc_nodes: &[DocNode]) {
    DiagnosticDocNodeVisitor { diagnostics: self }
      .visit_doc_nodes(doc_nodes.iter())
  }

  fn check_missing_js_doc(&mut self, js_doc: &JsDoc, location: &Location) {
    if js_doc.doc.is_none()
      && !has_ignorable_js_doc_tag(js_doc)
      && self.seen_jsdoc_missing.insert(location.clone())
    {
      if let Some(text_info) = self.maybe_get_text_info(location) {
        self.diagnostics.push(DocDiagnostic {
          location: location.clone(),
          kind: DocDiagnosticKind::MissingJsDoc,
          text_info,
        });
      }
    }
  }

  fn check_missing_explicit_type(
    &mut self,
    ts_type: Option<&TsTypeDef>,
    js_doc: &JsDoc,
    location: &Location,
  ) {
    if ts_type.is_none()
      && !has_ignorable_js_doc_tag(js_doc)
      && self.seen_missing_type_refs.insert(location.clone())
    {
      if let Some(text_info) = self.maybe_get_text_info(location) {
        self.diagnostics.push(DocDiagnostic {
          location: location.clone(),
          kind: DocDiagnosticKind::MissingExplicitType,
          text_info,
        })
      }
    }
  }

  fn check_missing_return_type(
    &mut self,
    return_type: Option<&TsTypeDef>,
    js_doc: &JsDoc,
    location: &Location,
  ) {
    if return_type.is_none()
      && !has_ignorable_js_doc_tag(js_doc)
      && self.seen_missing_type_refs.insert(location.clone())
    {
      if let Some(text_info) = self.maybe_get_text_info(location) {
        self.diagnostics.push(DocDiagnostic {
          location: location.clone(),
          kind: DocDiagnosticKind::MissingReturnType,
          text_info,
        });
      }
    }
  }

  fn maybe_get_text_info(&self, location: &Location) -> Option<SourceTextInfo> {
    fn try_get(
      root_symbol: &RootSymbol,
      location: &Location,
    ) -> Option<SourceTextInfo> {
      let specifier = ModuleSpecifier::parse(&location.filename).ok()?;
      Some(
        root_symbol
          .module_from_specifier(&specifier)?
          .text_info()
          .clone(),
      )
    }

    match try_get(&self.root_symbol, location) {
      Some(text_info) => Some(text_info),
      None => {
        // should never happen
        debug_assert!(
          false,
          "Failed to get text info for {}",
          location.filename
        );
        None
      }
    }
  }
}

struct DiagnosticDocNodeVisitor<'a, 'b> {
  diagnostics: &'a mut DiagnosticsCollector<'b>,
}

impl DiagnosticDocNodeVisitor<'_, '_> {
  pub fn visit_doc_nodes<'c, I>(&'c mut self, doc_nodes: I)
  where
    I: Iterator<Item = &'c DocNode>,
  {
    let mut last_node: Option<&DocNode> = None;
    for doc_node in doc_nodes {
      if !doc_node.location.filename.starts_with("file:") {
        continue; // don't report diagnostics on remote modules
      }

      if let Some(last_node) = last_node {
        if doc_node.name == last_node.name && last_node.function_def().is_some()
        {
          if let Some(current_fn) = &doc_node.function_def() {
            if current_fn.has_body {
              continue; // it's an overload. Ignore it
            }
          }
        }
      }

      if !has_ignorable_js_doc_tag(&doc_node.js_doc) {
        self.visit_doc_node(doc_node);
      }

      last_node = Some(doc_node);
    }
  }

  fn visit_doc_node(&mut self, doc_node: &DocNode) {
    fn is_js_docable_kind(node: &DocNode) -> bool {
      match node.def {
        DocNodeDef::Class { .. }
        | DocNodeDef::Enum { .. }
        | DocNodeDef::Function { .. }
        | DocNodeDef::Interface { .. }
        | DocNodeDef::Namespace { .. }
        | DocNodeDef::TypeAlias { .. }
        | DocNodeDef::Variable { .. } => true,
        DocNodeDef::Import { .. }
        | DocNodeDef::ModuleDoc { .. }
        | DocNodeDef::Reference { .. } => false,
      }
    }

    if doc_node.declaration_kind == DeclarationKind::Private {
      return; // skip, we don't do these diagnostics above private nodes
    }

    if is_js_docable_kind(doc_node) {
      self
        .diagnostics
        .check_missing_js_doc(&doc_node.js_doc, &doc_node.location);
    }

    if let Some(def) = &doc_node.class_def() {
      self.visit_class_def(def);
    }

    if let Some(def) = &doc_node.function_def() {
      self.visit_function_def(doc_node, def);
    }

    if let Some(def) = &doc_node.interface_def() {
      self.visit_interface_def(def);
    }

    if let Some(def) = &doc_node.namespace_def() {
      self.visit_namespace_def(def);
    }

    if let Some(def) = &doc_node.variable_def() {
      self.visit_variable_def(doc_node, def);
    }
  }

  fn visit_class_def(&mut self, def: &crate::class::ClassDef) {
    // ctors
    if def.constructors.len() == 1 {
      self.visit_class_ctor_def(&def.constructors[0]);
    } else if !def.constructors.is_empty() {
      // skip the first one
      let ctors = &def.constructors[1..];
      for ctor in ctors {
        self.visit_class_ctor_def(ctor);
      }
    }

    // properties
    for prop in def.properties.iter() {
      if prop.accessibility == Some(Accessibility::Private) {
        continue; // don't do diagnostics for private types
      }
      self
        .diagnostics
        .check_missing_js_doc(&prop.js_doc, &prop.location);
      self.diagnostics.check_missing_explicit_type(
        prop.ts_type.as_ref(),
        &prop.js_doc,
        &prop.location,
      )
    }

    // index signatures
    for sig in def.index_signatures.iter() {
      self
        .diagnostics
        .check_missing_js_doc(&sig.js_doc, &sig.location);
      self.diagnostics.check_missing_explicit_type(
        sig.ts_type.as_ref(),
        &sig.js_doc,
        &sig.location,
      )
    }

    // methods
    let mut last_name: Option<&str> = None;
    for method in def.methods.iter() {
      if let Some(last_name) = last_name {
        if &*method.name == last_name && method.function_def.has_body {
          continue; // skip, it's the implementation signature
        }
      }

      self
        .diagnostics
        .check_missing_js_doc(&method.js_doc, &method.location);
      self.diagnostics.check_missing_return_type(
        method.function_def.return_type.as_ref(),
        &method.js_doc,
        &method.location,
      );

      last_name = Some(&method.name);
    }
  }

  fn visit_class_ctor_def(&mut self, ctor: &crate::class::ClassConstructorDef) {
    // Don't require a jsdoc for private constructors or constructors
    // with no parameters.
    if ctor.accessibility == Some(Accessibility::Private)
      || ctor.params.is_empty()
    {
      return;
    }
    self
      .diagnostics
      .check_missing_js_doc(&ctor.js_doc, &ctor.location);
  }

  fn visit_function_def(
    &mut self,
    parent: &DocNode,
    def: &crate::function::FunctionDef,
  ) {
    self
      .diagnostics
      .check_missing_js_doc(&parent.js_doc, &parent.location);
    self.diagnostics.check_missing_return_type(
      def.return_type.as_ref(),
      &parent.js_doc,
      &parent.location,
    );
  }

  fn visit_interface_def(&mut self, def: &crate::interface::InterfaceDef) {
    // constructors
    for constructor in &def.constructors {
      self
        .diagnostics
        .check_missing_js_doc(&constructor.js_doc, &constructor.location);
      self.diagnostics.check_missing_return_type(
        constructor.return_type.as_ref(),
        &constructor.js_doc,
        &constructor.location,
      );
    }

    // properties
    for prop in &def.properties {
      self
        .diagnostics
        .check_missing_js_doc(&prop.js_doc, &prop.location);

      self.diagnostics.check_missing_explicit_type(
        prop.ts_type.as_ref(),
        &prop.js_doc,
        &prop.location,
      )
    }

    // index signatures
    for sig in &def.index_signatures {
      self
        .diagnostics
        .check_missing_js_doc(&sig.js_doc, &sig.location);
      self.diagnostics.check_missing_explicit_type(
        sig.ts_type.as_ref(),
        &sig.js_doc,
        &sig.location,
      );
    }

    // methods
    for method in &def.methods {
      self
        .diagnostics
        .check_missing_js_doc(&method.js_doc, &method.location);
      self.diagnostics.check_missing_return_type(
        method.return_type.as_ref(),
        &method.js_doc,
        &method.location,
      );
    }
  }

  fn visit_namespace_def(&mut self, def: &NamespaceDef) {
    self.visit_doc_nodes(def.elements.iter().map(|element| element.as_ref()));
  }

  fn visit_variable_def(&mut self, parent: &DocNode, def: &VariableDef) {
    self.diagnostics.check_missing_explicit_type(
      def.ts_type.as_ref(),
      &parent.js_doc,
      &parent.location,
    );
  }
}

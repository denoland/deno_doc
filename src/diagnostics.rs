// Copyright 2020-2023 the Deno authors. All rights reserved. MIT license.

use crate::Location;
use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;
use crate::node::DeclarationDef;
use crate::node::DeclarationKind;
use crate::node::NamespaceDef;
use crate::node::Symbol;
use crate::ts_type::TsTypeDef;
use crate::util::swc::get_text_info_location;
use crate::util::swc::has_ignorable_js_doc_tag;
use crate::util::symbol::symbol_has_ignorable_js_doc_tag;
use crate::variable::VariableDef;

use deno_ast::ModuleSpecifier;
use deno_ast::SourceRange;
use deno_ast::SourceTextInfo;
use deno_ast::diagnostics::Diagnostic;
use deno_ast::diagnostics::DiagnosticLevel;
use deno_ast::diagnostics::DiagnosticLocation;
use deno_ast::diagnostics::DiagnosticSnippet;
use deno_ast::diagnostics::DiagnosticSnippetHighlight;
use deno_ast::diagnostics::DiagnosticSnippetHighlightStyle;
use deno_ast::diagnostics::DiagnosticSourcePos;
use deno_ast::diagnostics::DiagnosticSourceRange;
use deno_ast::swc::ast::Accessibility;
use deno_graph::symbols::ModuleInfoRef;
use deno_graph::symbols::RootSymbol;
use deno_graph::symbols::Symbol as GraphSymbol;
use deno_graph::symbols::UniqueSymbolId;
use std::sync::Arc;

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

  fn location(&self) -> DiagnosticLocation<'_> {
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

/// Whether a JSDoc block documents its symbol through a tag, even though it has
/// no top-level description.
///
/// A `@deprecated` notice that carries an explanation counts as documentation.
/// When `@deprecated` is the first block of a comment, any description that
/// follows it is absorbed into the tag (per JSDoc semantics, the description
/// must precede all tags), so the symbol ends up with no `doc` field even
/// though it is clearly documented. Treating such a symbol as missing JSDoc
/// would be a false positive.
fn has_documenting_tag(js_doc: &JsDoc) -> bool {
  js_doc.tags.iter().any(|tag| {
    matches!(
      tag,
      JsDocTag::Deprecated { doc: Some(doc) } if !doc.trim().is_empty()
    )
  })
}

/// Whether `c` may appear within a JavaScript/TypeScript identifier, used to
/// find identifier boundaries when refining a diagnostic's location.
fn is_identifier_char(c: char) -> bool {
  c == '_' || c == '$' || c.is_alphanumeric()
}

/// Whether `token` is a keyword or modifier that may appear between the start
/// of a declaration or member and its identifier.
fn is_modifier_keyword(token: &str) -> bool {
  matches!(
    token,
    "abstract"
      | "accessor"
      | "async"
      | "await"
      | "class"
      | "const"
      | "declare"
      | "default"
      | "enum"
      | "export"
      | "function"
      | "get"
      | "interface"
      | "let"
      | "module"
      | "namespace"
      | "new"
      | "override"
      | "private"
      | "protected"
      | "public"
      | "readonly"
      | "set"
      | "static"
      | "type"
      | "using"
      | "var"
  )
}

/// Advances past whitespace and comments, returning the byte index of the next
/// token or `None` when the source ends or a comment is unterminated.
fn skip_whitespace_and_comments(text: &str, mut index: usize) -> Option<usize> {
  loop {
    let rest = text.get(index..)?;
    index += rest.len() - rest.trim_start().len();
    let rest = text.get(index..)?;
    if let Some(comment) = rest.strip_prefix("//") {
      index += 2 + comment.find('\n').map_or(comment.len(), |i| i + 1);
    } else if let Some(comment) = rest.strip_prefix("/*") {
      index += 2 + comment.find("*/")? + 2;
    } else if rest.is_empty() {
      return None;
    } else {
      return Some(index);
    }
  }
}

/// Advances past a balanced bracket pair starting at `index`, skipping over
/// string literals, and returns the byte index just after the closing bracket.
fn skip_balanced(text: &str, index: usize) -> Option<usize> {
  let mut chars = text.get(index..)?.char_indices();
  let mut depth: usize = 0;
  while let Some((offset, c)) = chars.next() {
    match c {
      '(' | '[' | '{' => depth += 1,
      ')' | ']' | '}' => {
        depth = depth.checked_sub(1)?;
        if depth == 0 {
          return Some(index + offset + c.len_utf8());
        }
      }
      quote @ ('"' | '\'' | '`') => {
        let mut escaped = false;
        loop {
          let (_, c) = chars.next()?;
          if escaped {
            escaped = false;
          } else if c == '\\' {
            escaped = true;
          } else if c == quote {
            break;
          }
        }
      }
      _ => {}
    }
  }
  None
}

/// Advances past a decorator (`@name`, `@ns.name`, `@name(...)`), returning the
/// byte index just after it.
fn skip_decorator(text: &str, index: usize) -> Option<usize> {
  let mut index = skip_whitespace_and_comments(text, index + '@'.len_utf8())?;
  loop {
    let rest = text.get(index..)?;
    let len = rest
      .find(|c: char| !is_identifier_char(c))
      .unwrap_or(rest.len());
    if len == 0 {
      return None;
    }
    index = skip_whitespace_and_comments(text, index + len)?;
    match text.get(index..)?.chars().next()? {
      // a qualified name, e.g. `@ns.deco`
      '.' => index = skip_whitespace_and_comments(text, index + 1)?,
      // the decorator's arguments, which end it
      '(' | '[' => return skip_balanced(text, index),
      _ => return Some(index),
    }
  }
}

/// Advances past whitespace, comments and decorators, returning the byte index
/// of the next token.
fn skip_trivia(text: &str, mut index: usize) -> Option<usize> {
  loop {
    index = skip_whitespace_and_comments(text, index)?;
    if !text.get(index..)?.starts_with('@') {
      return Some(index);
    }
    index = skip_decorator(text, index)?;
  }
}

/// Finds the byte index of the identifier of the declaration or member
/// starting at `start`. See [`identifier_location`].
fn identifier_offset(text: &str, start: usize, name: &str) -> Option<usize> {
  if name.is_empty() {
    return None;
  }
  // a default export is named `default`, which is a keyword rather than the
  // declaration's identifier, so accept whichever identifier it declares
  let is_default = name == "default";
  let mut index = start;
  loop {
    index = skip_trivia(text, index)?;
    let rest = text.get(index..)?;
    let first = rest.chars().next()?;
    // the `*` of a generator and the `?`/`!` of an optional or definite member
    if matches!(first, '*' | '?' | '!') {
      index += first.len_utf8();
      continue;
    }
    if first.is_numeric() || !(first == '#' || is_identifier_char(first)) {
      return None;
    }
    let tail = &rest[first.len_utf8()..];
    let len = first.len_utf8()
      + tail
        .find(|c: char| !is_identifier_char(c))
        .unwrap_or(tail.len());
    let token = &rest[..len];
    let is_identifier = if is_default {
      !is_modifier_keyword(token)
    } else {
      token == name
    };
    if is_identifier {
      return Some(index);
    }
    if !is_modifier_keyword(token) {
      // a segment of a qualified name, e.g. the `RootNs` of
      // `namespace RootNs.OtherNs`, whose doc node is named `OtherNs`
      let after = skip_whitespace_and_comments(text, index + len)?;
      if text.get(after..)?.starts_with('.') {
        index = after + '.'.len_utf8();
        continue;
      }
      // anything else means this declaration's identifier isn't `name`, so
      // there is nothing to refine
      return None;
    }
    index += len;
  }
}

/// Refines a declaration-start `location` to point at the declaration's
/// identifier.
///
/// `deno doc --lint` diagnostics are anchored at the start of a declaration,
/// which for many forms (`export class Foo`, `get foo()`, `@deco foo`, …) is a
/// keyword or a decorator rather than the symbol's name. Only trivia,
/// decorators and modifier keywords separate a declaration's start from its
/// identifier, so tokenizing forward from the declaration start finds the
/// identifier without ever leaving the declaration's own header.
///
/// Returns the original location unchanged when `name` is empty (e.g. index
/// signatures, which have no identifier) and when the declaration's identifier
/// isn't `name` — which is the case for aliased exports (`export { foo as bar
/// }`), where `name` is the alias while the location is the declaration's.
fn identifier_location(
  text_info: &SourceTextInfo,
  location: &Location,
  name: &str,
) -> Location {
  let text = text_info.text_str();
  let Some(offset) = identifier_offset(text, location.byte_index, name) else {
    return location.clone();
  };
  let pos = text_info.range().start + offset;
  get_text_info_location(&location.filename, text_info, pos)
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
    referenced_symbol: &GraphSymbol,
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
              line: 0,
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

  pub fn analyze_doc_nodes(&mut self, doc_nodes: &[Arc<Symbol>]) {
    DiagnosticDocNodeVisitor { diagnostics: self }
      .visit_doc_nodes(doc_nodes.iter().map(|s| &**s))
  }

  fn check_missing_js_doc(
    &mut self,
    js_doc: &JsDoc,
    location: &Location,
    name: &str,
  ) {
    if js_doc.doc.is_none()
      && !has_documenting_tag(js_doc)
      && !has_ignorable_js_doc_tag(js_doc)
      && self.seen_jsdoc_missing.insert(location.clone())
      && let Some(text_info) = self.maybe_get_text_info(location)
    {
      self.diagnostics.push(DocDiagnostic {
        location: identifier_location(&text_info, location, name),
        kind: DocDiagnosticKind::MissingJsDoc,
        text_info,
      });
    }
  }

  fn check_missing_explicit_type(
    &mut self,
    ts_type: Option<&TsTypeDef>,
    js_doc: &JsDoc,
    location: &Location,
    name: &str,
  ) {
    if ts_type.is_none()
      && !has_ignorable_js_doc_tag(js_doc)
      && self.seen_missing_type_refs.insert(location.clone())
      && let Some(text_info) = self.maybe_get_text_info(location)
    {
      self.diagnostics.push(DocDiagnostic {
        location: identifier_location(&text_info, location, name),
        kind: DocDiagnosticKind::MissingExplicitType,
        text_info,
      })
    }
  }

  fn check_missing_return_type(
    &mut self,
    return_type: Option<&TsTypeDef>,
    js_doc: &JsDoc,
    location: &Location,
    name: &str,
  ) {
    if return_type.is_none()
      && !has_ignorable_js_doc_tag(js_doc)
      && self.seen_missing_type_refs.insert(location.clone())
      && let Some(text_info) = self.maybe_get_text_info(location)
    {
      self.diagnostics.push(DocDiagnostic {
        location: identifier_location(&text_info, location, name),
        kind: DocDiagnosticKind::MissingReturnType,
        text_info,
      });
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
    I: Iterator<Item = &'c Symbol>,
  {
    for doc_node in doc_nodes {
      let fn_decl_count = doc_node
        .declarations
        .iter()
        .filter(|d| matches!(d.def, DeclarationDef::Function(..)))
        .count();
      let has_fn_overloads = fn_decl_count > 1;

      for (i, decl) in doc_node.declarations.iter().enumerate() {
        if !decl.location.filename.starts_with("file:") {
          continue; // don't report diagnostics on remote modules
        }

        if has_fn_overloads
          && decl.function_def().is_some_and(|def| def.has_body)
          && i > 0
        {
          continue; // it's an overload. Ignore it
        }

        if !has_ignorable_js_doc_tag(&decl.js_doc) {
          self.visit_decl(&doc_node.name, decl);
        }
      }
    }
  }

  fn visit_decl(&mut self, name: &str, decl: &crate::node::Declaration) {
    fn is_js_docable_kind(def: &DeclarationDef) -> bool {
      match def {
        DeclarationDef::Class(..)
        | DeclarationDef::Enum(..)
        | DeclarationDef::Function(..)
        | DeclarationDef::Interface(..)
        | DeclarationDef::Namespace(..)
        | DeclarationDef::TypeAlias(..)
        | DeclarationDef::Variable(..) => true,
        DeclarationDef::Reference(..) => false,
      }
    }

    if decl.declaration_kind == DeclarationKind::Private {
      return; // skip, we don't do these diagnostics above private nodes
    }

    if is_js_docable_kind(&decl.def) {
      self
        .diagnostics
        .check_missing_js_doc(&decl.js_doc, &decl.location, name);
    }

    if let Some(def) = &decl.class_def() {
      self.visit_class_def(def);
    }

    if let Some(def) = &decl.function_def() {
      self.visit_function_def(name, decl, def);
    }

    if let Some(def) = &decl.interface_def() {
      self.visit_interface_def(def);
    }

    if let Some(def) = &decl.namespace_def() {
      self.visit_namespace_def(def);
    }

    if let Some(def) = &decl.variable_def() {
      self.visit_variable_def(name, decl, def);
    }
  }

  fn visit_class_def(&mut self, def: &crate::class::ClassDef) {
    // ctors
    if def.constructors.len() == 1 {
      self.visit_class_ctor_def(&def.constructors[0]);
    } else if !def.constructors.is_empty() {
      for ctor in &def.constructors {
        if !ctor.has_body {
          self.visit_class_ctor_def(ctor);
        }
      }
    }

    // properties
    for prop in def.properties.iter() {
      if prop.accessibility == Some(Accessibility::Private) {
        continue; // don't do diagnostics for private types
      }
      self.diagnostics.check_missing_js_doc(
        &prop.js_doc,
        &prop.location,
        &prop.name,
      );
      self.diagnostics.check_missing_explicit_type(
        prop.ts_type.as_ref(),
        &prop.js_doc,
        &prop.location,
        &prop.name,
      )
    }

    // index signatures
    for sig in def.index_signatures.iter() {
      self
        .diagnostics
        .check_missing_js_doc(&sig.js_doc, &sig.location, "");
      self.diagnostics.check_missing_explicit_type(
        sig.ts_type.as_ref(),
        &sig.js_doc,
        &sig.location,
        "",
      )
    }

    // methods
    let mut last_name: Option<&str> = None;
    for method in def.methods.iter() {
      if let Some(last_name) = last_name
        && &*method.name == last_name
        && method.function_def.has_body
      {
        continue; // skip, it's the implementation signature
      }

      self.diagnostics.check_missing_js_doc(
        &method.js_doc,
        &method.location,
        &method.name,
      );
      self.diagnostics.check_missing_return_type(
        method.function_def.return_type.as_ref(),
        &method.js_doc,
        &method.location,
        &method.name,
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
    self.diagnostics.check_missing_js_doc(
      &ctor.js_doc,
      &ctor.location,
      &ctor.name,
    );
  }

  fn visit_function_def(
    &mut self,
    name: &str,
    decl: &crate::node::Declaration,
    def: &crate::function::FunctionDef,
  ) {
    self
      .diagnostics
      .check_missing_js_doc(&decl.js_doc, &decl.location, name);
    self.diagnostics.check_missing_return_type(
      def.return_type.as_ref(),
      &decl.js_doc,
      &decl.location,
      name,
    );
  }

  fn visit_interface_def(&mut self, def: &crate::interface::InterfaceDef) {
    // constructors
    for constructor in &def.constructors {
      self.diagnostics.check_missing_js_doc(
        &constructor.js_doc,
        &constructor.location,
        "",
      );
      self.diagnostics.check_missing_return_type(
        constructor.return_type.as_ref(),
        &constructor.js_doc,
        &constructor.location,
        "",
      );
    }

    // properties
    for prop in &def.properties {
      self.diagnostics.check_missing_js_doc(
        &prop.js_doc,
        &prop.location,
        &prop.name,
      );

      self.diagnostics.check_missing_explicit_type(
        prop.ts_type.as_ref(),
        &prop.js_doc,
        &prop.location,
        &prop.name,
      )
    }

    // index signatures
    for sig in &def.index_signatures {
      self
        .diagnostics
        .check_missing_js_doc(&sig.js_doc, &sig.location, "");
      self.diagnostics.check_missing_explicit_type(
        sig.ts_type.as_ref(),
        &sig.js_doc,
        &sig.location,
        "",
      );
    }

    // methods
    for method in &def.methods {
      self.diagnostics.check_missing_js_doc(
        &method.js_doc,
        &method.location,
        &method.name,
      );
      self.diagnostics.check_missing_return_type(
        method.return_type.as_ref(),
        &method.js_doc,
        &method.location,
        &method.name,
      );
    }
  }

  fn visit_namespace_def(&mut self, def: &NamespaceDef) {
    self.visit_doc_nodes(def.elements.iter().map(|element| element.as_ref()));
  }

  fn visit_variable_def(
    &mut self,
    name: &str,
    decl: &crate::node::Declaration,
    def: &VariableDef,
  ) {
    self.diagnostics.check_missing_explicit_type(
      def.ts_type.as_ref(),
      &decl.js_doc,
      &decl.location,
      name,
    );
  }
}

#[cfg(test)]
mod test {
  use super::identifier_offset;

  /// Returns the text from the identifier that `identifier_offset` finds for
  /// the declaration starting at the `<start>` marker in `text`.
  fn identifier(text: &str, name: &str) -> Option<String> {
    let start = text.find("<start>").expect("missing <start> marker");
    let text = text.replace("<start>", "");
    let offset = identifier_offset(&text, start, name)?;
    Some(
      text[offset..]
        .chars()
        .take_while(|c| super::is_identifier_char(*c))
        .collect(),
    )
  }

  #[test]
  fn finds_identifier_after_keywords() {
    assert_eq!(
      identifier("<start>export declare class Foo {}", "Foo").as_deref(),
      Some("Foo")
    );
    assert_eq!(
      identifier("class A {\n  <start>static get foo() {}\n}", "foo")
        .as_deref(),
      Some("foo")
    );
    // a member whose name happens to be a modifier keyword
    assert_eq!(
      identifier("interface A {\n  <start>get: string;\n}", "get").as_deref(),
      Some("get")
    );
  }

  #[test]
  fn skips_decorators_and_comments() {
    let text =
      "class A {\n  <start>@deco(\"value\") /* c */ value: string = \"x\";\n}";
    let offset = identifier_offset(
      &text.replace("<start>", ""),
      text.find("<start>").unwrap(),
      "value",
    );
    // the match must be the property, not the decorator's string argument
    assert_eq!(
      offset,
      Some(text.replace("<start>", "").find("value: string").unwrap())
    );
  }

  #[test]
  fn default_export_uses_the_declared_identifier() {
    assert_eq!(
      identifier("<start>export default class Foo {}", "default").as_deref(),
      Some("Foo")
    );
    // nothing to anchor on when the declaration is anonymous
    assert_eq!(
      identifier("<start>export default class {}", "default"),
      None
    );
  }

  #[test]
  fn stops_at_the_declaration_it_starts_at() {
    // `bar` is an export alias for `foo`, so the scan must not run on into the
    // unrelated `bar` declaration below
    let text = "<start>declare function foo(): void;\ndeclare const bar: number;\nexport { foo as bar };";
    assert_eq!(identifier(text, "bar"), None);
  }
}

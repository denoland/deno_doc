// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::Location;
use crate::display::SliceDisplayer;
use crate::display::display_abstract;
use crate::display::display_async;
use crate::display::display_generator;
use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;
use crate::node::DeclarationKind;
use crate::node::DocNode;
use crate::node::DocNodeDef;

use deno_terminal::colors;
use deno_terminal::colors::Style;

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;

fn italic_cyan<'a, S: Display + 'a>(s: S) -> Style<Style<S>> {
  colors::italic(colors::cyan(s))
}

pub struct DocPrinter<'a> {
  doc_nodes: &'a [DocNode],
  use_color: bool,
  private: bool,
}

impl DocPrinter<'_> {
  pub fn new(
    doc_nodes: &[DocNode],
    use_color: bool,
    private: bool,
  ) -> DocPrinter<'_> {
    DocPrinter {
      doc_nodes,
      use_color,
      private,
    }
  }

  pub fn format(&self, w: &mut Formatter<'_>) -> FmtResult {
    self.format_with_indent(w, self.doc_nodes, 0)
  }

  fn format_with_indent(
    &self,
    w: &mut Formatter<'_>,
    doc_nodes: &[DocNode],
    indent: i64,
  ) -> FmtResult {
    colors::set_use_color(self.use_color);

    let mut sorted = Vec::from(doc_nodes);
    sorted.sort_unstable_by(|a, b| {
      let kind_cmp = self.kind_order(a).cmp(&self.kind_order(b));
      if kind_cmp == core::cmp::Ordering::Equal {
        a.name.cmp(&b.name)
      } else {
        kind_cmp
      }
    });

    for node in &sorted {
      let has_overloads = if matches!(node.def, DocNodeDef::Function { .. }) {
        sorted
          .iter()
          .filter(|n| {
            matches!(n.def, DocNodeDef::Function { .. }) && n.name == node.name
          })
          .count()
          > 1
      } else {
        false
      };

      if !has_overloads
        || node
          .function_def()
          .map(|def| !def.has_body)
          .unwrap_or(false)
      {
        write!(
          w,
          "{}",
          colors::italic_gray(&format!(
            "Defined in {}\n\n",
            get_location_string(&node.location),
          ))
        )?;
      }

      self.format_signature(w, node, indent, has_overloads)?;

      self.format_jsdoc(w, &node.js_doc, indent + 1)?;
      writeln!(w)?;

      match node.def {
        DocNodeDef::Class { .. } => self.format_class(w, node)?,
        DocNodeDef::Enum { .. } => self.format_enum(w, node)?,
        DocNodeDef::Interface { .. } => self.format_interface(w, node)?,
        DocNodeDef::Namespace { .. } => self.format_namespace(w, node)?,
        _ => {}
      }
    }

    if self.use_color {
      colors::set_use_color(false);
    }

    Ok(())
  }

  fn kind_order(&self, node: &DocNode) -> i64 {
    match node.def {
      DocNodeDef::ModuleDoc => 0,
      DocNodeDef::Function { .. } => 1,
      DocNodeDef::Variable { .. } => 2,
      DocNodeDef::Class { .. } => 3,
      DocNodeDef::Enum { .. } => 4,
      DocNodeDef::Interface { .. } => 5,
      DocNodeDef::TypeAlias { .. } => 6,
      DocNodeDef::Namespace { .. } => 7,
      DocNodeDef::Import { .. } => 8,
      DocNodeDef::Reference { .. } => 9,
    }
  }

  fn format_signature(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
    indent: i64,
    has_overloads: bool,
  ) -> FmtResult {
    match node.def {
      DocNodeDef::ModuleDoc => self.format_module_doc(w, node, indent),
      DocNodeDef::Function { .. } => {
        self.format_function_signature(w, node, indent, has_overloads)
      }
      DocNodeDef::Variable { .. } => {
        self.format_variable_signature(w, node, indent)
      }
      DocNodeDef::Class { .. } => self.format_class_signature(w, node, indent),
      DocNodeDef::Enum { .. } => self.format_enum_signature(w, node, indent),
      DocNodeDef::Interface { .. } => {
        self.format_interface_signature(w, node, indent)
      }
      DocNodeDef::TypeAlias { .. } => {
        self.format_type_alias_signature(w, node, indent)
      }
      DocNodeDef::Namespace { .. } => {
        self.format_namespace_signature(w, node, indent)
      }
      DocNodeDef::Import { .. } => Ok(()),
      DocNodeDef::Reference { .. } => {
        self.format_reference_signature(w, node, indent)
      }
    }
  }

  fn format_jsdoc(
    &self,
    w: &mut Formatter<'_>,
    js_doc: &JsDoc,
    indent: i64,
  ) -> FmtResult {
    if let Some(doc) = &js_doc.doc {
      render_markdown(w, doc, indent)?;
    }
    if !js_doc.tags.is_empty() {
      writeln!(w)?;
    }
    for tag in js_doc.tags.iter() {
      self.format_jsdoc_tag(w, tag, indent)?;
    }
    Ok(())
  }

  fn format_jsdoc_tag_maybe_doc(
    &self,
    w: &mut Formatter<'_>,
    maybe_doc: &Option<Box<str>>,
    indent: i64,
  ) -> FmtResult {
    if let Some(doc) = maybe_doc {
      render_markdown(w, doc, indent + 2)?;
      writeln!(w)
    } else {
      Ok(())
    }
  }

  fn format_jsdoc_tag_doc(
    &self,
    w: &mut Formatter<'_>,
    doc: &str,
    indent: i64,
  ) -> FmtResult {
    render_markdown(w, doc, indent + 2)?;
    writeln!(w)
  }

  fn format_jsdoc_tag(
    &self,
    w: &mut Formatter<'_>,
    tag: &JsDocTag,
    indent: i64,
  ) -> FmtResult {
    match tag {
      JsDocTag::Callback { name, doc } => {
        writeln!(
          w,
          "{}@{} {}",
          Indent(indent),
          colors::magenta("callback"),
          colors::bold(name)
        )?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::Category { doc } => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("category"))?;
        self.format_jsdoc_tag_doc(w, doc, indent)
      }
      JsDocTag::Constructor => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("constructor"))
      }
      JsDocTag::Default { value, doc } => {
        writeln!(
          w,
          "{}@{} {{{}}}",
          Indent(indent),
          colors::magenta("default"),
          italic_cyan(value),
        )?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::Deprecated { doc } => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("deprecated"))?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::Enum { type_ref, doc } => {
        writeln!(
          w,
          "{}@{} {{{}}}",
          Indent(indent),
          colors::magenta("enum"),
          italic_cyan(type_ref),
        )?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::Example { doc } => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("example"))?;
        self.format_jsdoc_tag_doc(w, doc, indent)
      }
      JsDocTag::Experimental => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("experimental"))
      }
      JsDocTag::Extends { type_ref, doc } => {
        writeln!(
          w,
          "{}@{} {{{}}}",
          Indent(indent),
          colors::magenta("extends"),
          italic_cyan(type_ref)
        )?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::Ignore => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("ignore"))
      }
      JsDocTag::Internal => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("internal"))
      }
      JsDocTag::Module { name } => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("module"))?;
        self.format_jsdoc_tag_maybe_doc(w, name, indent)
      }
      JsDocTag::Param {
        name,
        type_ref,
        optional,
        default,
        doc,
      } => {
        write!(w, "{}@{}", Indent(indent), colors::magenta("param"))?;
        if let Some(type_ref) = type_ref {
          write!(w, " {{{}}}", italic_cyan(type_ref))?;
        }
        if *optional {
          write!(w, " [?]")?;
        } else if let Some(default) = default {
          write!(w, " [{}]", italic_cyan(default))?;
        }
        writeln!(w, " {}", colors::bold(name))?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::Public => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("public"))
      }
      JsDocTag::Private => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("private"))
      }
      JsDocTag::Property {
        name,
        type_ref,
        doc,
      } => {
        writeln!(
          w,
          "{}@{} {{{}}} {}",
          Indent(indent),
          colors::magenta("property"),
          italic_cyan(type_ref),
          colors::bold(name)
        )?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::Protected => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("protected"))
      }
      JsDocTag::ReadOnly => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("readonly"))
      }
      JsDocTag::Return { type_ref, doc } => {
        write!(w, "{}@{}", Indent(indent), colors::magenta("return"))?;
        if let Some(type_ref) = type_ref {
          writeln!(w, " {{{}}}", italic_cyan(type_ref))?;
        } else {
          writeln!(w)?;
        }
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::Tags { tags } => {
        writeln!(
          w,
          "{}@{} {}",
          Indent(indent),
          colors::magenta("tags"),
          tags.join(", "),
        )
      }
      JsDocTag::Template { name, doc } => {
        writeln!(
          w,
          "{}@{} {}",
          Indent(indent),
          colors::magenta("template"),
          colors::bold(name)
        )?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::This { type_ref, doc } => {
        writeln!(
          w,
          "{}@{} {{{}}}",
          Indent(indent),
          colors::magenta("this"),
          italic_cyan(type_ref)
        )?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::TypeDef {
        name,
        type_ref,
        doc,
      } => {
        writeln!(
          w,
          "{}@{} {{{}}} {}",
          Indent(indent),
          colors::magenta("typedef"),
          italic_cyan(type_ref),
          colors::bold(name)
        )?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::TypeRef { type_ref, doc } => {
        writeln!(
          w,
          "{}@{} {{{}}}",
          Indent(indent),
          colors::magenta("typeref"),
          italic_cyan(type_ref)
        )?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::Unsupported { value } => {
        let name = value.split_whitespace().next().unwrap_or("@");
        let value = &value[name.len()..];
        writeln!(
          w,
          "{}@{}{}",
          Indent(indent),
          colors::magenta(&name[1..]),
          value
        )
      }
      JsDocTag::See { doc } => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("see"))?;
        self.format_jsdoc_tag_doc(w, doc, indent)
      }
      JsDocTag::Since { doc } => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("since"))?;
        self.format_jsdoc_tag_doc(w, doc, indent)
      }
      JsDocTag::Priority { priority } => {
        writeln!(
          w,
          "{}@{} {{{}}}",
          Indent(indent),
          colors::magenta("priority"),
          italic_cyan(priority),
        )
      }
      JsDocTag::Throws { type_ref, doc } => {
        write!(w, "{}@{}", Indent(indent), colors::magenta("return"))?;
        if let Some(type_ref) = type_ref {
          writeln!(w, " {{{}}}", italic_cyan(type_ref))?;
        } else {
          writeln!(w)?;
        }
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
    }
  }

  fn format_class(&self, w: &mut Formatter<'_>, node: &DocNode) -> FmtResult {
    let class_def = node.class_def().unwrap();
    let has_overloads = class_def.constructors.len() > 1;
    for node in class_def.constructors.iter() {
      if !has_overloads || !node.has_body {
        writeln!(w, "{}{}", Indent(1), node,)?;
        self.format_jsdoc(w, &node.js_doc, 2)?;
      }
    }
    for node in class_def.properties.iter().filter(|node| {
      self.private
        || node
          .accessibility
          .unwrap_or(deno_ast::swc::ast::Accessibility::Public)
          != deno_ast::swc::ast::Accessibility::Private
    }) {
      for d in node.decorators.iter() {
        writeln!(w, "{}{}", Indent(1), d)?;
      }
      writeln!(w, "{}{}", Indent(1), node,)?;
      self.format_jsdoc(w, &node.js_doc, 2)?;
    }
    for index_sign_def in class_def.index_signatures.iter() {
      writeln!(w, "{}{}", Indent(1), index_sign_def)?;
    }
    for node in class_def.methods.iter().filter(|node| {
      self.private
        || node
          .accessibility
          .unwrap_or(deno_ast::swc::ast::Accessibility::Public)
          != deno_ast::swc::ast::Accessibility::Private
    }) {
      let has_overloads = class_def
        .methods
        .iter()
        .filter(|n| n.name == node.name)
        .count()
        > 1;
      if !has_overloads || !node.function_def.has_body {
        for d in node.function_def.decorators.iter() {
          writeln!(w, "{}{}", Indent(1), d)?;
        }
        writeln!(w, "{}{}", Indent(1), node,)?;
        self.format_jsdoc(w, &node.js_doc, 2)?;
      }
    }
    writeln!(w)
  }

  fn format_enum(&self, w: &mut Formatter<'_>, node: &DocNode) -> FmtResult {
    let enum_def = node.enum_def().unwrap();
    for member in &enum_def.members {
      writeln!(w, "{}{}", Indent(1), colors::bold(&member.name))?;
      self.format_jsdoc(w, &member.js_doc, 2)?;
    }
    writeln!(w)
  }

  fn format_interface(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
  ) -> FmtResult {
    let interface_def = node.interface_def().unwrap();

    for constructor in &interface_def.constructors {
      writeln!(w, "{}{}", Indent(1), constructor)?;
      self.format_jsdoc(w, &constructor.js_doc, 2)?;
    }
    for property_def in &interface_def.properties {
      writeln!(w, "{}{}", Indent(1), property_def)?;
      self.format_jsdoc(w, &property_def.js_doc, 2)?;
    }
    for method_def in &interface_def.methods {
      writeln!(w, "{}{}", Indent(1), method_def)?;
      self.format_jsdoc(w, &method_def.js_doc, 2)?;
    }
    for index_sign_def in &interface_def.index_signatures {
      writeln!(w, "{}{}", Indent(1), index_sign_def)?;
    }
    writeln!(w)
  }

  fn format_namespace(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
  ) -> FmtResult {
    let elements = &node.namespace_def().unwrap().elements;
    for node in elements {
      let has_overloads = if matches!(node.def, DocNodeDef::Function { .. }) {
        elements
          .iter()
          .filter(|n| {
            matches!(n.def, DocNodeDef::Function { .. }) && n.name == node.name
          })
          .count()
          > 1
      } else {
        false
      };
      self.format_signature(w, node, 1, has_overloads)?;
      self.format_jsdoc(w, &node.js_doc, 2)?;
    }
    writeln!(w)
  }

  fn format_class_signature(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
    indent: i64,
  ) -> FmtResult {
    let class_def = node.class_def().unwrap();
    for node in class_def.decorators.iter() {
      writeln!(w, "{}{}", Indent(indent), node)?;
    }
    write!(
      w,
      "{}{}{}{} {}",
      Indent(indent),
      fmt_visibility(node.declaration_kind),
      display_abstract(class_def.is_abstract),
      colors::magenta("class"),
      colors::bold(&node.name),
    )?;
    if !class_def.type_params.is_empty() {
      write!(
        w,
        "<{}>",
        SliceDisplayer::new(&class_def.type_params, ", ", false)
      )?;
    }

    if let Some(extends) = &class_def.extends {
      write!(w, " {} {}", colors::magenta("extends"), extends)?;
    }
    if !class_def.super_type_params.is_empty() {
      write!(
        w,
        "<{}>",
        SliceDisplayer::new(&class_def.super_type_params, ", ", false)
      )?;
    }

    if !class_def.implements.is_empty() {
      write!(
        w,
        " {} {}",
        colors::magenta("implements"),
        SliceDisplayer::new(&class_def.implements, ", ", false)
      )?;
    }

    writeln!(w)
  }

  fn format_enum_signature(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
    indent: i64,
  ) -> FmtResult {
    writeln!(
      w,
      "{}{}{} {}",
      Indent(indent),
      fmt_visibility(node.declaration_kind),
      colors::magenta("enum"),
      colors::bold(&node.name)
    )
  }

  fn format_function_signature(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
    indent: i64,
    has_overloads: bool,
  ) -> FmtResult {
    let function_def = node.function_def().unwrap();
    if !has_overloads || !function_def.has_body {
      write!(
        w,
        "{}{}{}{}{} {}",
        Indent(indent),
        fmt_visibility(node.declaration_kind),
        display_async(function_def.is_async),
        colors::magenta("function"),
        display_generator(function_def.is_generator),
        colors::bold(&node.name)
      )?;
      if !function_def.type_params.is_empty() {
        write!(
          w,
          "<{}>",
          SliceDisplayer::new(&function_def.type_params, ", ", false)
        )?;
      }
      write!(
        w,
        "({})",
        SliceDisplayer::new(&function_def.params, ", ", false)
      )?;
      if let Some(return_type) = &function_def.return_type {
        write!(w, ": {}", return_type)?;
      }
      writeln!(w)?;
    }
    Ok(())
  }

  fn format_interface_signature(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
    indent: i64,
  ) -> FmtResult {
    let interface_def = node.interface_def().unwrap();
    write!(
      w,
      "{}{}{} {}",
      Indent(indent),
      fmt_visibility(node.declaration_kind),
      colors::magenta("interface"),
      colors::bold(&node.name)
    )?;

    if !interface_def.type_params.is_empty() {
      write!(
        w,
        "<{}>",
        SliceDisplayer::new(&interface_def.type_params, ", ", false)
      )?;
    }

    if !interface_def.extends.is_empty() {
      write!(
        w,
        " {} {}",
        colors::magenta("extends"),
        SliceDisplayer::new(&interface_def.extends, ", ", false)
      )?;
    }

    writeln!(w)
  }

  fn format_module_doc(
    &self,
    _w: &mut Formatter<'_>,
    _node: &DocNode,
    _indent: i64,
  ) -> FmtResult {
    // currently we do not print out JSDoc in the printer, so there is nothing
    // to print.
    Ok(())
  }

  fn format_type_alias_signature(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
    indent: i64,
  ) -> FmtResult {
    let type_alias_def = node.type_alias_def().unwrap();
    write!(
      w,
      "{}{}{} {}",
      Indent(indent),
      fmt_visibility(node.declaration_kind),
      colors::magenta("type"),
      colors::bold(&node.name),
    )?;

    if !type_alias_def.type_params.is_empty() {
      write!(
        w,
        "<{}>",
        SliceDisplayer::new(&type_alias_def.type_params, ", ", false)
      )?;
    }

    writeln!(w, " = {}", type_alias_def.ts_type)
  }

  fn format_namespace_signature(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
    indent: i64,
  ) -> FmtResult {
    writeln!(
      w,
      "{}{}{} {}",
      Indent(indent),
      fmt_visibility(node.declaration_kind),
      colors::magenta("namespace"),
      colors::bold(&node.name)
    )
  }

  fn format_reference_signature(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
    indent: i64,
  ) -> FmtResult {
    let reference_def = node.reference_def().unwrap();

    writeln!(
      w,
      "{}{}{} {}: {}",
      Indent(indent),
      fmt_visibility(node.declaration_kind),
      colors::magenta("reference"),
      colors::bold(&node.name),
      colors::italic_gray(get_location_string(&reference_def.target)),
    )
  }

  fn format_variable_signature(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
    indent: i64,
  ) -> FmtResult {
    let variable_def = node.variable_def().unwrap();
    write!(
      w,
      "{}{}{} {}",
      Indent(indent),
      fmt_visibility(node.declaration_kind),
      colors::magenta(match variable_def.kind {
        deno_ast::swc::ast::VarDeclKind::Const => "const",
        deno_ast::swc::ast::VarDeclKind::Let => "let",
        deno_ast::swc::ast::VarDeclKind::Var => "var",
      }),
      colors::bold(&node.name),
    )?;
    if let Some(ts_type) = &variable_def.ts_type {
      write!(w, ": {}", ts_type)?;
    }
    writeln!(w)
  }
}

impl Display for DocPrinter<'_> {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    self.format(f)
  }
}

fn fmt_visibility(decl_kind: DeclarationKind) -> impl std::fmt::Display {
  colors::italic_gray(if decl_kind == DeclarationKind::Private {
    "private "
  } else {
    ""
  })
}

fn get_location_string(location: &Location) -> String {
  format!(
    "{}:{}:{}",
    location.filename,
    location.line,
    // todo(#150): for some reason the column is 0-indexed and the line
    // is 1-indexed. Display them both as 1-indexed so that vscode goes
    // to the correct column when clicking this.
    location.col + 1
  )
}

struct Indent(pub i64);

impl Display for Indent {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    for _ in 0..self.0 {
      write!(f, "  ")?;
    }
    Ok(())
  }
}

#[cfg(not(feature = "comrak"))]
fn render_markdown(
  w: &mut Formatter<'_>,
  markdown: &str,
  indent: i64,
) -> FmtResult {
  for line in markdown.lines() {
    writeln!(w, "{}{}", Indent(indent), colors::gray(line))?;
  }
  Ok(())
}

#[cfg(feature = "comrak")]
fn render_markdown(
  w: &mut Formatter<'_>,
  markdown: &str,
  indent: i64,
) -> FmtResult {
  use comrak::Arena;
  use comrak::arena_tree::NodeEdge;
  use comrak::nodes::ListType;
  use comrak::nodes::NodeValue;

  struct MarkdownRenderer<'a, 'b> {
    w: &'a mut Formatter<'b>,
    indent: i64,
    /// stack for inline text accumulation; nested inline nodes push/pop layers
    text_stack: Vec<String>,
    /// tracks nested list contexts for bullet/number rendering
    list_stack: Vec<ListContext>,
    /// url of the current link being processed
    link_url: Option<String>,
    /// depth of nested blockquotes for prefix rendering
    block_quote_depth: usize,
    /// whether the next content should be preceded by an indent
    at_line_start: bool,
    /// deferred blank line separator between block-level elements
    pending_blank_line: bool,
  }

  struct ListContext {
    list_type: ListType,
    current_item: usize,
  }

  impl MarkdownRenderer<'_, '_> {
    /// emits a blank line separator if one is pending from a previous block
    fn write_block_separator(&mut self) -> FmtResult {
      if self.pending_blank_line {
        self.pending_blank_line = false;
        self.push_output("\n")?;
      }
      Ok(())
    }

    /// writes text to the formatter with indent and blockquote prefixes
    fn push_output(&mut self, text: &str) -> FmtResult {
      let bq_prefix = if self.block_quote_depth > 0 {
        let p = format!("{} ", "│".repeat(self.block_quote_depth));
        Some(colors::gray(&p).to_string())
      } else {
        None
      };

      for (i, segment) in text.split('\n').enumerate() {
        if i > 0 {
          self.w.write_str("\n")?;
          self.at_line_start = true;
        }

        let has_bq = bq_prefix.is_some() && (!segment.is_empty() || i > 0);
        if (!segment.is_empty() || has_bq) && self.at_line_start {
          write!(self.w, "{}", Indent(self.indent))?;
          self.at_line_start = false;
        }
        if let Some(ref prefix) = bq_prefix
          && has_bq
        {
          self.w.write_str(prefix)?;
        }
        if !segment.is_empty() {
          self.w.write_str(segment)?;
        }
      }
      Ok(())
    }

    /// flushes accumulated inline text from the stack to the formatter
    fn flush_text(&mut self) -> FmtResult {
      if self.text_stack.len() > 1 {
        let text = self.text_stack.pop().unwrap_or_default();
        if let Some(top) = self.text_stack.last_mut() {
          top.push_str(&text);
        }
      } else if let Some(top) = self.text_stack.last_mut() {
        let text = std::mem::take(top);
        if !text.is_empty() {
          self.push_output(&text)?;
        }
      }
      Ok(())
    }

    fn push_inline(&mut self, text: &str) {
      if let Some(top) = self.text_stack.last_mut() {
        top.push_str(text);
      }
    }

    fn handle_start(&mut self, value: &NodeValue) -> FmtResult {
      match value {
        NodeValue::Document | NodeValue::FrontMatter(_) => {}
        NodeValue::Heading(_) => {
          self.write_block_separator()?;
          self.text_stack.push(String::new());
        }
        NodeValue::Paragraph => {
          self.write_block_separator()?;
        }
        NodeValue::Text(s) => {
          self.push_inline(s);
        }
        NodeValue::SoftBreak => {
          self.push_inline("\n");
        }
        NodeValue::LineBreak => {
          self.push_inline("\n");
        }
        NodeValue::Code(c) => {
          let styled = format!("`{}`", colors::cyan(&c.literal));
          self.push_inline(&styled);
        }
        NodeValue::CodeBlock(cb) => {
          self.write_block_separator()?;
          self.flush_text()?;
          let fence_open = if cb.info.is_empty() {
            "```".to_string()
          } else {
            format!("```{}", cb.info)
          };
          self.push_output(&format!("{}\n", colors::gray(&fence_open)))?;
          for line in cb.literal.lines() {
            self.push_output(&format!("{}\n", colors::gray(line)))?;
          }
          self.push_output(&format!("{}\n", colors::gray("```")))?;
          self.pending_blank_line = true;
        }
        NodeValue::Strong
        | NodeValue::Superscript
        | NodeValue::Underline
        | NodeValue::SpoileredText => {
          self.text_stack.push(String::new());
        }
        NodeValue::Emph => {
          self.text_stack.push(String::new());
        }
        NodeValue::Strikethrough => {
          self.text_stack.push(String::new());
        }
        NodeValue::Link(nl) => {
          self.link_url = Some(nl.url.clone());
          self.text_stack.push(String::new());
        }
        NodeValue::Image(nl) => {
          self.link_url = Some(nl.url.clone());
          self.text_stack.push(String::new());
        }
        NodeValue::WikiLink(wl) => {
          self.link_url = Some(wl.url.clone());
          self.text_stack.push(String::new());
        }
        NodeValue::BlockQuote | NodeValue::MultilineBlockQuote(_) => {
          self.write_block_separator()?;
          self.block_quote_depth += 1;
        }
        NodeValue::List(nl) => {
          self.write_block_separator()?;
          self.list_stack.push(ListContext {
            list_type: nl.list_type,
            current_item: nl.start,
          });
        }
        NodeValue::Item(_) => {
          let indent = "  ".repeat(self.list_stack.len().saturating_sub(1));
          if let Some(ctx) = self.list_stack.last_mut() {
            let marker = match ctx.list_type {
              ListType::Bullet => "- ".to_string(),
              ListType::Ordered => {
                let m = format!("{}. ", ctx.current_item);
                ctx.current_item += 1;
                m
              }
            };
            self.flush_text()?;
            self.push_output(&indent)?;
            self.push_output(&marker)?;
          }
        }
        NodeValue::TaskItem(checked) => {
          let checkbox = if checked.is_some() {
            colors::green("[x] ").to_string()
          } else {
            "[ ] ".to_string()
          };
          self.push_inline(&checkbox);
        }
        NodeValue::ThematicBreak => {
          self.write_block_separator()?;
          self.flush_text()?;
          let rule = "─".repeat(40);
          self.push_output(&format!("{}\n", colors::gray(&rule)))?;
          self.pending_blank_line = true;
        }
        NodeValue::HtmlBlock(hb) => {
          self.write_block_separator()?;
          self.push_output(&colors::dimmed_gray(&hb.literal).to_string())?;
          self.pending_blank_line = true;
        }
        NodeValue::HtmlInline(s) => {
          self.push_inline(&colors::dimmed_gray(s).to_string());
        }
        // leaf nodes with literal text content
        NodeValue::Math(m) => {
          self.push_inline(&colors::cyan(&m.literal).to_string());
        }
        NodeValue::FootnoteReference(r) => {
          self.push_inline(&format!("[^{}]", r.ix));
        }
        NodeValue::EscapedTag(s) => {
          self.push_inline(s);
        }
        // container nodes whose children provide the text
        NodeValue::Escaped
        | NodeValue::Table(_)
        | NodeValue::TableRow(_)
        | NodeValue::TableCell
        | NodeValue::DescriptionList
        | NodeValue::DescriptionItem(_)
        | NodeValue::DescriptionTerm
        | NodeValue::DescriptionDetails
        | NodeValue::FootnoteDefinition(_) => {}
      }
      Ok(())
    }

    fn handle_end(&mut self, value: &NodeValue) -> FmtResult {
      match value {
        NodeValue::Heading(h) => {
          let heading_text = self.text_stack.pop().unwrap_or_default();
          let prefix = "#".repeat(h.level as usize);
          let full = format!("{} {}", prefix, heading_text.trim());
          self.push_output(&colors::bold(&full).to_string())?;
          self.push_output("\n")?;
          self.pending_blank_line = true;
        }
        NodeValue::Paragraph => {
          self.flush_text()?;
          self.push_output("\n")?;
          if self.list_stack.is_empty() {
            self.pending_blank_line = true;
          }
        }
        NodeValue::Strong => {
          let text = self.text_stack.pop().unwrap_or_default();
          self.push_inline(&colors::bold(&text).to_string());
        }
        NodeValue::Emph => {
          let text = self.text_stack.pop().unwrap_or_default();
          self.push_inline(&colors::italic(&text).to_string());
        }
        NodeValue::Strikethrough => {
          let text = self.text_stack.pop().unwrap_or_default();
          self.push_inline(&format!("~{}~", text));
        }
        NodeValue::Superscript | NodeValue::SpoileredText => {
          // no terminal representation; just pop the accumulated text back
          let text = self.text_stack.pop().unwrap_or_default();
          self.push_inline(&text);
        }
        NodeValue::Underline => {
          // no terminal underline in deno_terminal::colors; just pop text back
          let text = self.text_stack.pop().unwrap_or_default();
          self.push_inline(&text);
        }
        NodeValue::Link(_) | NodeValue::WikiLink(_) => {
          let text = self.text_stack.pop().unwrap_or_default();
          let url = self.link_url.take().unwrap_or_default();
          let styled = if text == url || text.is_empty() {
            colors::cyan_with_underline(&url).to_string()
          } else {
            format!("{} ({})", text, colors::cyan_with_underline(&url))
          };
          self.push_inline(&styled);
        }
        NodeValue::Image(_) => {
          let alt_text = self.text_stack.pop().unwrap_or_default();
          let url = self.link_url.take().unwrap_or_default();
          let display = if alt_text.is_empty() {
            format!("[image]({})", url)
          } else {
            format!("[{}]({})", alt_text, url)
          };
          self.push_inline(&colors::italic(&display).to_string());
        }
        NodeValue::BlockQuote | NodeValue::MultilineBlockQuote(_) => {
          self.block_quote_depth = self.block_quote_depth.saturating_sub(1);
        }
        NodeValue::List(_) => {
          self.list_stack.pop();
          if self.list_stack.is_empty() {
            self.pending_blank_line = true;
          }
        }
        NodeValue::Item(_) => {
          self.flush_text()?;
          if !self.at_line_start {
            self.w.write_str("\n")?;
            self.at_line_start = true;
          }
        }
        NodeValue::TableRow(_) => {
          self.flush_text()?;
          self.push_output("\n")?;
        }
        NodeValue::TableCell => {
          self.flush_text()?;
          self.push_output("\t")?;
        }
        NodeValue::DescriptionTerm => {
          self.flush_text()?;
          self.push_output("\n")?;
        }
        NodeValue::DescriptionDetails => {
          self.flush_text()?;
          self.push_output("\n")?;
        }
        // leaf nodes, no children to close
        NodeValue::Document
        | NodeValue::FrontMatter(_)
        | NodeValue::Text(_)
        | NodeValue::SoftBreak
        | NodeValue::LineBreak
        | NodeValue::Code(_)
        | NodeValue::CodeBlock(_)
        | NodeValue::ThematicBreak
        | NodeValue::HtmlBlock(_)
        | NodeValue::HtmlInline(_)
        | NodeValue::TaskItem(_)
        | NodeValue::Math(_)
        | NodeValue::FootnoteReference(_)
        | NodeValue::EscapedTag(_) => {}
        // container nodes with no special end handling
        NodeValue::Escaped
        | NodeValue::Table(_)
        | NodeValue::DescriptionList
        | NodeValue::DescriptionItem(_)
        | NodeValue::FootnoteDefinition(_) => {}
      }
      Ok(())
    }
  }

  let arena = Arena::new();
  let options = comrak::Options::default();
  let root = comrak::parse_document(&arena, markdown, &options);

  let mut renderer = MarkdownRenderer {
    w,
    indent,
    text_stack: vec![String::new()],
    list_stack: Vec::new(),
    link_url: None,
    block_quote_depth: 0,
    at_line_start: true,
    pending_blank_line: false,
  };

  for edge in root.traverse() {
    match edge {
      NodeEdge::Start(node) => {
        let data = node.data.borrow();
        renderer.handle_start(&data.value)?;
      }
      NodeEdge::End(node) => {
        let data = node.data.borrow();
        renderer.handle_end(&data.value)?;
      }
    }
  }

  // flush any remaining text
  renderer.flush_text()?;
  Ok(())
}

#[cfg(test)]
mod render_markdown_tests {
  use super::*;
  use std::fmt;

  struct Rendered<'a> {
    markdown: &'a str,
    indent: i64,
  }

  impl fmt::Display for Rendered<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      render_markdown(f, self.markdown, self.indent)
    }
  }

  fn render(markdown: &str) -> String {
    colors::set_use_color(false);
    format!(
      "{}",
      Rendered {
        markdown,
        indent: 0
      }
    )
  }

  fn render_indented(markdown: &str, indent: i64) -> String {
    colors::set_use_color(false);
    format!("{}", Rendered { markdown, indent })
  }

  #[test]
  fn plain_text() {
    let output = render("hello world");
    assert_eq!(output, "hello world\n");
  }

  #[test]
  fn multiple_paragraphs() {
    let output = render("first paragraph\n\nsecond paragraph");
    assert_eq!(output, "first paragraph\n\nsecond paragraph\n");
  }

  #[test]
  fn heading_levels() {
    assert_eq!(render("# h1"), "# h1\n");
    assert_eq!(render("## h2"), "## h2\n");
    assert_eq!(render("### h3"), "### h3\n");
  }

  #[test]
  fn heading_then_paragraph() {
    let output = render("# title\n\nbody text");
    assert_eq!(output, "# title\n\nbody text\n");
  }

  #[test]
  fn bold_text() {
    let output = render("some **bold** text");
    assert_eq!(output, "some bold text\n");
  }

  #[test]
  fn italic_text() {
    let output = render("some *italic* text");
    assert_eq!(output, "some italic text\n");
  }

  #[test]
  fn inline_code() {
    let output = render("use `fmt::Display`");
    assert_eq!(output, "use `fmt::Display`\n");
  }

  #[test]
  fn code_block() {
    let output = render("```ts\nconst x = 1;\n```");
    assert_eq!(output, "```ts\nconst x = 1;\n```\n");
  }

  #[test]
  fn code_block_no_lang() {
    let output = render("```\nplain code\n```");
    assert_eq!(output, "```\nplain code\n```\n");
  }

  #[test]
  fn link_with_text() {
    let output = render("[click here](https://example.com)");
    assert_eq!(output, "click here (https://example.com)\n");
  }

  #[test]
  fn link_bare_url() {
    // when link text matches url, only the url is shown
    let output = render("[https://example.com](https://example.com)");
    assert_eq!(output, "https://example.com\n");
  }

  #[test]
  fn image() {
    let output = render("![alt text](img.png)");
    assert_eq!(output, "[alt text](img.png)\n");
  }

  #[test]
  fn unordered_list() {
    let output = render("- one\n- two\n- three");
    assert_eq!(output, "- one\n- two\n- three\n");
  }

  #[test]
  fn ordered_list() {
    let output = render("1. first\n2. second\n3. third");
    assert_eq!(output, "1. first\n2. second\n3. third\n");
  }

  #[test]
  fn nested_list() {
    let output = render("- outer\n  - inner\n- back");
    assert_eq!(output, "- outer\n  - inner\n- back\n");
  }

  #[test]
  fn task_list() {
    let output = render("- [x] done\n- [ ] pending");
    assert_eq!(output, "- [x] done\n- [ ] pending\n");
  }

  #[test]
  fn blockquote() {
    let output = render("> quoted text");
    assert_eq!(output, "│ quoted text\n│ ");
  }

  #[test]
  fn nested_blockquote() {
    let output = render("> outer\n>> inner");
    assert_eq!(output, "│ outer\n│ \n│ ││ inner\n││ ");
  }

  #[test]
  fn thematic_break() {
    let output = render("above\n\n---\n\nbelow");
    let expected = format!("above\n\n{}\n\nbelow\n", "─".repeat(40));
    assert_eq!(output, expected);
  }

  #[test]
  fn strikethrough() {
    // comrak's default options don't enable the strikethrough extension,
    // so ~~text~~ is treated as literal text
    let output = render("some ~~deleted~~ text");
    assert_eq!(output, "some ~~deleted~~ text\n");
  }

  #[test]
  fn html_inline() {
    let output = render("text <br> more");
    assert_eq!(output, "text <br> more\n");
  }

  #[test]
  fn soft_break_preserves_newline() {
    // a single newline inside a paragraph is a soft break; we preserve it
    // rather than joining into one line, to respect the original wrapping
    let output = render("line one\nline two");
    assert_eq!(output, "line one\nline two\n");
  }

  #[test]
  fn indent_applied() {
    let output = render_indented("hello", 2);
    assert_eq!(output, "    hello\n");
  }

  #[test]
  fn indent_on_list() {
    let output = render_indented("- a\n- b", 1);
    assert_eq!(output, "  - a\n  - b\n");
  }

  #[test]
  fn empty_input() {
    let output = render("");
    assert_eq!(output, "");
  }
}

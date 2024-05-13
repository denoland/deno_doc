// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::panic::AssertUnwindSafe;

use deno_ast::diagnostics::Diagnostic;
use deno_doc::DocNode;
use deno_graph::source::Source;
use deno_graph::ModuleSpecifier;
use file_test_runner::collect_and_run_tests;
use file_test_runner::collection::strategies::TestPerFileCollectionStrategy;
use file_test_runner::collection::CollectOptions;
use file_test_runner::collection::CollectedTest;
use file_test_runner::RunOptions;
use file_test_runner::TestResult;
use indexmap::IndexMap;
use pretty_assertions::assert_eq;
use serde::Deserialize;
use serde::Serialize;

use crate::helpers::TestBuilder;

mod helpers;

fn main() {
  // run tests with `UPDATE=1` to automatically update the spec files
  collect_and_run_tests(
    CollectOptions {
      base: "tests/specs".into(),
      strategy: Box::new(TestPerFileCollectionStrategy { file_pattern: None }),
      filter_override: None,
    },
    RunOptions { parallel: true },
    |test| {
      TestResult::from_maybe_panic(AssertUnwindSafe(|| {
        run_test(test);
      }))
    },
  )
}

fn run_test(test: &CollectedTest) {
  let file_text = test.read_to_string().unwrap();
  let spec = parse_spec(file_text);

  let mut builder = TestBuilder::new();
  builder
    .with_loader(|loader| {
      for file in &spec.files {
        let source = Source::Module {
          specifier: file.url().to_string(),
          maybe_headers: Some(file.headers.clone().into_iter().collect()),
          content: file.text.clone(),
        };
        loader.add_source(file.url(), source);
      }
    })
    .set_private(spec.private);

  let rt = tokio::runtime::Builder::new_current_thread()
    .enable_all()
    .build()
    .unwrap();
  let result = rt.block_on(async { builder.build().await });
  let update_var = std::env::var("UPDATE");
  let mut json_output =
    serde_json::to_string_pretty(&result.json_output).unwrap();
  json_output.push('\n');
  let diagnostics = result
    .diagnostics
    .iter()
    .map(|d| {
      console_static_text::ansi::strip_ansi_codes(&d.display().to_string())
        .to_string()
    })
    .collect::<Vec<_>>()
    .join("\n");
  let spec = if update_var.as_ref().map(|v| v.as_str()) == Ok("1") {
    let mut spec = spec;
    spec.output_json_file.text = json_output.clone();
    spec.output_doc_file.text = result.text_output.clone();
    spec.diagnostics = diagnostics.clone();
    std::fs::write(&test.path, spec.emit()).unwrap();
    spec
  } else {
    spec
  };
  assert_eq!(
    result.text_output,
    spec.output_doc_file.text,
    "Should be same for doc output {}",
    test.path.display()
  );
  assert_eq!(
    json_output,
    spec.output_json_file.text,
    "Should be same for json output {}",
    test.path.display()
  );
  assert_eq!(
    diagnostics,
    spec.diagnostics,
    "Should be same for {}",
    test.path.display()
  );

  // Check that the JSON output is round-trippable.
  let _parsed_json_output: Vec<DocNode> =
    serde_json::from_str(&json_output).unwrap();
}

pub struct Spec {
  pub private: bool,
  pub files: Vec<SpecFile>,
  pub output_json_file: SpecFile,
  pub output_doc_file: SpecFile,
  pub diagnostics: String,
}

impl Spec {
  pub fn emit(&self) -> String {
    let mut text = String::new();
    if self.private {
      text.push_str("{ \"private\": true }\n");
    }
    for file in &self.files {
      text.push_str(&file.emit());
      text.push('\n');
    }
    if !self.diagnostics.is_empty() {
      text.push_str("# diagnostics\n");
      text.push_str(&self.diagnostics);
      text.push('\n');
    }
    text.push_str(&self.output_doc_file.emit());
    text.push('\n');
    text.push_str(&self.output_json_file.emit());
    text
  }
}

#[derive(Debug, Serialize, Deserialize)]
struct SpecOptions {
  pub private: bool,
}

#[derive(Debug)]
pub struct SpecFile {
  pub specifier: String,
  pub text: String,
  pub headers: IndexMap<String, String>,
}

impl SpecFile {
  pub fn emit(&self) -> String {
    let mut text = format!("# {}\n", self.specifier);
    if !self.headers.is_empty() {
      text.push_str(&format!(
        "HEADERS: {}\n",
        serde_json::to_string(&self.headers).unwrap()
      ));
    }
    text.push_str(&self.text);
    text
  }

  pub fn url(&self) -> ModuleSpecifier {
    let specifier = &self.specifier;
    if !specifier.starts_with("http") && !specifier.starts_with("file") {
      ModuleSpecifier::parse(&format!("file:///{}", specifier)).unwrap()
    } else {
      ModuleSpecifier::parse(specifier).unwrap()
    }
  }
}

fn parse_spec(text: String) -> Spec {
  let mut files = Vec::new();
  let mut current_file = None;
  let mut options: Option<SpecOptions> = None;
  for (i, line) in text.split('\n').enumerate() {
    if i == 0 && line.starts_with('{') {
      options = Some(serde_json::from_str(line).unwrap());
      continue;
    }
    if let Some(specifier) = line.strip_prefix("# ") {
      if let Some(file) = current_file.take() {
        files.push(file);
      }
      current_file = Some(SpecFile {
        specifier: specifier.to_string(),
        text: String::new(),
        headers: Default::default(),
      });
    } else if let Some(headers) = line.strip_prefix("HEADERS: ") {
      current_file.as_mut().unwrap().headers =
        serde_json::from_str(headers).unwrap();
    } else {
      let current_file = current_file.as_mut().unwrap();
      if !current_file.text.is_empty() {
        current_file.text.push('\n');
      }
      current_file.text.push_str(line);
    }
  }
  files.push(current_file.unwrap());
  let output_json_file = files.remove(
    files
      .iter()
      .position(|f| f.specifier == "output.json")
      .unwrap(),
  );
  let output_doc_file = files.remove(
    files
      .iter()
      .position(|f| f.specifier == "output.txt")
      .unwrap(),
  );
  let diagnostics = take_text_file(&mut files, "diagnostics");
  Spec {
    private: options.map(|o| o.private).unwrap_or(false),
    files,
    output_json_file,
    output_doc_file,
    diagnostics,
  }
}

fn take_text_file(files: &mut Vec<SpecFile>, name: &str) -> String {
  if let Some(index) = files.iter().position(|f| f.specifier == name) {
    let file = files.remove(index);
    file.text
  } else {
    String::new()
  }
}

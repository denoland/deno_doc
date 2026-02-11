// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::panic::AssertUnwindSafe;

use deno_doc::diff::DocDiff;
use deno_graph::ModuleSpecifier;
use deno_graph::source::Source;
use file_test_runner::RunOptions;
use file_test_runner::TestResult;
use file_test_runner::collect_and_run_tests;
use file_test_runner::collection::CollectOptions;
use file_test_runner::collection::CollectedTest;
use file_test_runner::collection::strategies::TestPerFileCollectionStrategy;
use pretty_assertions::assert_eq;

use crate::helpers::DiffTestBuilder;

mod helpers;

fn main() {
  collect_and_run_tests(
    CollectOptions {
      base: "tests/diff_specs".into(),
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

  let mut builder = DiffTestBuilder::new();
  builder.with_loader(|loader| {
    for file in &spec.old_files {
      let source = Source::Module {
        specifier: file.url().to_string(),
        maybe_headers: None,
        content: file.text.clone(),
      };
      loader.add_source(file.url(), source);
    }
  });

  let rt = tokio::runtime::Builder::new_current_thread()
    .enable_all()
    .build()
    .unwrap();

  let old_docs = rt.block_on(builder.build());

  let mut builder = DiffTestBuilder::new();
  builder.with_loader(|loader| {
    for file in &spec.new_files {
      let source = Source::Module {
        specifier: file.url().to_string(),
        maybe_headers: None,
        content: file.text.clone(),
      };
      loader.add_source(file.url(), source);
    }
  });

  let new_docs = rt.block_on(builder.build());

  let diff = DocDiff::diff(&old_docs, &new_docs);

  let update_var = std::env::var("UPDATE");
  let mut json_output = serde_json::to_string_pretty(&diff).unwrap();
  json_output.push('\n');

  let spec = if update_var.as_ref().map(|v| v.as_str()) == Ok("1") {
    let mut spec = spec;
    spec.output_json_file.text = json_output.clone();
    std::fs::write(&test.path, spec.emit()).unwrap();
    spec
  } else {
    spec
  };

  assert_eq!(
    json_output,
    spec.output_json_file.text,
    "Should be same for {}",
    test.path.display()
  );

  // Check that the JSON output is round-trippable.
  let _parsed: DocDiff = serde_json::from_str(&json_output).unwrap();
}

pub struct DiffSpec {
  pub old_files: Vec<SpecFile>,
  pub new_files: Vec<SpecFile>,
  pub output_json_file: SpecFile,
}

impl DiffSpec {
  pub fn emit(&self) -> String {
    let mut text = String::new();
    for file in &self.old_files {
      text.push_str(&format!("# old/{}\n", file.specifier));
      text.push_str(&file.text);
      text.push('\n');
    }
    for file in &self.new_files {
      text.push_str(&format!("# new/{}\n", file.specifier));
      text.push_str(&file.text);
      text.push('\n');
    }
    text.push_str(&self.output_json_file.emit());
    text
  }
}

#[derive(Debug)]
pub struct SpecFile {
  pub specifier: String,
  pub text: String,
}

impl SpecFile {
  pub fn emit(&self) -> String {
    format!("# {}\n{}", self.specifier, self.text)
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

fn parse_spec(text: String) -> DiffSpec {
  let mut old_files = Vec::new();
  let mut new_files = Vec::new();
  let mut current_file: Option<(SpecFile, bool)> = None; // (file, is_old)

  for line in text.split('\n') {
    if let Some(rest) = line.strip_prefix("# old/") {
      if let Some((file, is_old)) = current_file.take() {
        if is_old {
          old_files.push(file);
        } else {
          new_files.push(file);
        }
      }
      current_file = Some((
        SpecFile {
          specifier: rest.to_string(),
          text: String::new(),
        },
        true,
      ));
    } else if let Some(rest) = line.strip_prefix("# new/") {
      if let Some((file, is_old)) = current_file.take() {
        if is_old {
          old_files.push(file);
        } else {
          new_files.push(file);
        }
      }
      current_file = Some((
        SpecFile {
          specifier: rest.to_string(),
          text: String::new(),
        },
        false,
      ));
    } else if let Some(rest) = line.strip_prefix("# ") {
      if let Some((file, is_old)) = current_file.take() {
        if is_old {
          old_files.push(file);
        } else {
          new_files.push(file);
        }
      }
      current_file = Some((
        SpecFile {
          specifier: rest.to_string(),
          text: String::new(),
        },
        false, // output files go to new_files temporarily
      ));
    } else if let Some((ref mut file, _)) = current_file {
      if !file.text.is_empty() {
        file.text.push('\n');
      }
      file.text.push_str(line);
    }
  }

  if let Some((file, is_old)) = current_file.take() {
    if is_old {
      old_files.push(file);
    } else {
      new_files.push(file);
    }
  }

  let output_json_file = new_files
    .iter()
    .position(|f| f.specifier == "output.json")
    .map(|i| new_files.remove(i))
    .expect("output.json not found");

  DiffSpec {
    old_files,
    new_files,
    output_json_file,
  }
}

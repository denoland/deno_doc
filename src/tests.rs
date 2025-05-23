// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::node::DocNodeDef;
use crate::parser::DocParser;
use crate::printer::DocPrinter;
use crate::DocParserOptions;
use deno_graph::source::MemoryLoader;
use deno_graph::source::Source;
use deno_graph::BuildOptions;
use deno_graph::CapturingModuleAnalyzer;
use deno_graph::GraphKind;
use deno_graph::ModuleGraph;
use deno_graph::ModuleSpecifier;
use pretty_assertions::assert_eq;
use serde_json::json;

type MaybeHeaders<S> = Option<Vec<(S, S)>>;

pub(crate) async fn setup<S: AsRef<str> + Copy>(
  root: S,
  sources: Vec<(S, MaybeHeaders<S>, S)>,
) -> (ModuleGraph, CapturingModuleAnalyzer, ModuleSpecifier) {
  let sources = sources
    .into_iter()
    .map(|(s, h, c)| {
      (
        s,
        Source::Module {
          specifier: s,
          maybe_headers: h,
          content: c,
        },
      )
    })
    .collect();
  let memory_loader = MemoryLoader::new(sources, vec![]);
  let root = ModuleSpecifier::parse(root.as_ref()).unwrap();
  let analyzer = create_analyzer();
  let mut graph = ModuleGraph::new(GraphKind::TypesOnly);
  graph
    .build(
      vec![root.clone()],
      Vec::new(),
      &memory_loader,
      BuildOptions {
        module_analyzer: &analyzer,
        ..Default::default()
      },
    )
    .await;
  (graph, analyzer, root)
}

#[tokio::test]
async fn content_type_handling() {
  let sources = vec![(
    "https://example.com/a",
    Source::Module {
      specifier: "https://example.com/a",
      maybe_headers: Some(vec![(
        "content-type",
        "application/typescript; charset=utf-8",
      )]),
      content: r#"export interface A {
      a: string;
    }"#,
    },
  )];
  let memory_loader = MemoryLoader::new(sources, vec![]);
  let root = ModuleSpecifier::parse("https://example.com/a").unwrap();
  let analyzer = create_analyzer();
  let mut graph = ModuleGraph::new(GraphKind::TypesOnly);
  graph
    .build(
      vec![root.clone()],
      Vec::new(),
      &memory_loader,
      BuildOptions {
        module_analyzer: &analyzer,
        ..Default::default()
      },
    )
    .await;
  let entries =
    DocParser::new(&graph, &analyzer, &[root], DocParserOptions::default())
      .unwrap()
      .parse()
      .unwrap()
      .into_values()
      .next()
      .unwrap();
  assert_eq!(entries.len(), 1);
}

#[tokio::test]
async fn types_header_handling() {
  let sources = vec![
    (
      "https://example.com/a.js",
      Source::Module {
        specifier: "https://example.com/a.js",
        maybe_headers: Some(vec![
          ("content-type", "application/javascript; charset=utf-8"),
          ("x-typescript-types", "./a.d.ts"),
        ]),
        content: r#"console.log("a");"#,
      },
    ),
    (
      "https://example.com/a.d.ts",
      Source::Module {
        specifier: "https://example.com/a.d.ts",
        maybe_headers: Some(vec![(
          "content-type",
          "application/typescript; charset=utf-8",
        )]),
        content: r#"export const a: "a";"#,
      },
    ),
  ];
  let memory_loader = MemoryLoader::new(sources, vec![]);
  let root = ModuleSpecifier::parse("https://example.com/a.js").unwrap();
  let analyzer = create_analyzer();
  let mut graph = ModuleGraph::new(GraphKind::TypesOnly);
  graph
    .build(
      vec![root.clone()],
      Vec::new(),
      &memory_loader,
      BuildOptions {
        module_analyzer: &analyzer,
        ..Default::default()
      },
    )
    .await;
  let entries =
    DocParser::new(&graph, &analyzer, &[root], DocParserOptions::default())
      .unwrap()
      .parse()
      .unwrap()
      .into_values()
      .next()
      .unwrap();
  assert_eq!(
    serde_json::to_value(&entries).unwrap(),
    json!([{
      "kind": "variable",
      "name": "a",
      "isDefault": false,
      "location": {
        "filename": "https://example.com/a.d.ts",
        "line": 1,
        "col": 13,
        "byteIndex": 13
      },
      "declarationKind": "export",
      "variableDef": {
        "tsType": {
          "repr": "a",
          "kind": "literal",
          "literal": {
            "kind": "string",
            "string": "a"
          }
        },
        "kind": "const"
      }
    }])
  );
}

#[test]
fn location_byte_index_serde_default() {
  serde_json::from_value::<crate::Location>(json!({
    "filename": "",
    "line": 0,
    "col": 1,
  }))
  .unwrap();
}

#[tokio::test]
async fn reexports() {
  let nested_reexport_source_code = r#"
/**
  * JSDoc for bar
  */
export const bar = "bar";

export default 42;
"#;
  let reexport_source_code = r#"
export { bar } from "./nested_reexport.ts";

/**
 * JSDoc for const
 */
export const foo = "foo";

export const fizz = "fizz";
"#;
  let test_source_code = r#"
export { default, foo as fooConst, bar as barReExport } from "./reexport.ts";
import { fizz as buzz } from "./reexport.ts";

/** JSDoc for function */
export function fooFn(a: number) {
  return a;
}
"#;
  let (graph, analyzer, specifier) = setup(
    "file:///test.ts",
    vec![
      ("file:///test.ts", None, test_source_code),
      ("file:///reexport.ts", None, reexport_source_code),
      (
        "file:///nested_reexport.ts",
        None,
        nested_reexport_source_code,
      ),
    ],
  )
  .await;
  let entries = DocParser::new(
    &graph,
    &analyzer,
    &[specifier],
    DocParserOptions::default(),
  )
  .unwrap()
  .parse()
  .unwrap()
  .into_values()
  .next()
  .unwrap();

  let expected_json = json!([
    {
      "kind": "variable",
      "name": "fooConst",
      "isDefault": false,
      "location": {
        "filename": "file:///reexport.ts",
        "line": 7,
        "col": 13,
        "byteIndex": 86
      },
      "declarationKind": "export",
      "jsDoc": {
        "doc": "JSDoc for const",
      },
      "variableDef": {
        "tsType": {
          "repr": "foo",
          "kind": "literal",
          "literal": {
            "kind": "string",
            "string": "foo"
          }
        },
        "kind": "const"
      }
    },
    {
      "kind": "variable",
      "name": "barReExport",
      "isDefault": false,
      "location": {
        "filename": "file:///nested_reexport.ts",
        "line": 5,
        "col": 13,
        "byteIndex": 41
      },
      "declarationKind": "export",
      "jsDoc": {
        "doc": "JSDoc for bar",
      },
      "variableDef": {
        "tsType": {
          "repr": "bar",
          "kind": "literal",
          "literal": {
            "kind": "string",
            "string": "bar"
          }
        },
        "kind": "const"
      }
    },
    {
      "kind": "function",
      "name": "fooFn",
      "isDefault": false,
      "location": {
        "filename": "file:///test.ts",
        "line": 6,
        "col": 0,
        "byteIndex": 152
      },
      "declarationKind": "export",
      "jsDoc": {
        "doc": "JSDoc for function",
      },
      "functionDef": {
        "params": [
            {
              "name": "a",
              "kind": "identifier",
              "optional": false,
              "tsType": {
                "keyword": "number",
                "kind": "keyword",
                "repr": "number",
              },
            }
        ],
        "typeParams": [],
        "returnType": null,
        "hasBody": true,
        "isAsync": false,
        "isGenerator": false
      },
    },
    {
      "kind": "import",
      "name": "buzz",
      "location": {
        "filename": "file:///test.ts",
        "line": 3,
        "col": 0,
        "byteIndex": 79
      },
      "declarationKind": "private",
      "importDef": {
        "src": "file:///reexport.ts",
        "imported": "fizz",
      }
    }
  ]);
  let actual = serde_json::to_value(&entries).unwrap();
  assert_eq!(actual, expected_json);

  assert!(DocPrinter::new(&entries, false, false)
    .to_string()
    .as_str()
    .contains("function fooFn(a: number)"));
}

#[tokio::test]
async fn reexports_has_same_name() {
  let reexport_source_code = r#"
export interface Hello {}
export class Hello {}
"#;
  let test_source_code = r#"
export { Hello } from "./reexport.ts";
"#;

  let (graph, analyzer, specifier) = setup(
    "file:///test.ts",
    vec![
      ("file:///test.ts", None, test_source_code),
      ("file:///reexport.ts", None, reexport_source_code),
    ],
  )
  .await;
  let entries = DocParser::new(
    &graph,
    &analyzer,
    &[specifier],
    DocParserOptions::default(),
  )
  .unwrap()
  .parse()
  .unwrap()
  .into_values()
  .next()
  .unwrap();

  let expected_json = json!([
    {
      "kind": "interface",
      "name": "Hello",
      "isDefault": false,
      "location": {
        "filename": "file:///reexport.ts",
        "line": 2,
        "col": 0,
        "byteIndex": 1
      },
      "declarationKind": "export",
      "interfaceDef": {
        "extends": [],
        "constructors": [],
        "methods": [],
        "properties": [],
        "callSignatures": [],
        "indexSignatures": [],
        "typeParams": []
      }
    },
    {
      "kind": "class",
      "name": "Hello",
      "isDefault": false,
      "location": {
        "filename": "file:///reexport.ts",
        "line": 3,
        "col": 0,
        "byteIndex": 27
      },
      "declarationKind": "export",
      "classDef": {
        "isAbstract": false,
        "constructors": [],
        "properties": [],
        "indexSignatures": [],
        "methods": [],
        "extends": null,
        "implements": [],
        "typeParams": [],
        "superTypeParams": []
      }
    }
  ]);
  let actual = serde_json::to_value(&entries).unwrap();
  assert_eq!(actual, expected_json);

  let output = DocPrinter::new(&entries, false, false).to_string();
  assert!(output.contains("class Hello"));
  assert!(output.contains("interface Hello"));
}

#[tokio::test]
async fn deep_reexports() {
  let foo_source_code = r#"export const foo: string = "foo";"#;
  let bar_source_code = r#"export * from "./foo.ts""#;
  let baz_source_code = r#"export * from "./bar.ts""#;

  let (graph, analyzer, specifier) = setup(
    "file:///baz.ts",
    vec![
      ("file:///foo.ts", None, foo_source_code),
      ("file:///bar.ts", None, bar_source_code),
      ("file:///baz.ts", None, baz_source_code),
    ],
  )
  .await;
  let entries = DocParser::new(
    &graph,
    &analyzer,
    &[specifier],
    DocParserOptions::default(),
  )
  .unwrap()
  .parse()
  .unwrap()
  .into_values()
  .next()
  .unwrap();

  let expected_json = json!([
    {
      "kind": "variable",
      "name": "foo",
      "isDefault": false,
      "location": {
        "filename": "file:///foo.ts",
        "line": 1,
        "col": 13,
        "byteIndex": 13
      },
      "declarationKind": "export",
      "variableDef": {
        "tsType": {
          "repr": "string",
          "kind": "keyword",
          "keyword": "string"
        },
        "kind": "const"
      }
    }
  ]);
  let actual = serde_json::to_value(&entries).unwrap();
  assert_eq!(actual, expected_json);

  assert!(DocPrinter::new(&entries, false, false)
    .to_string()
    .contains("const foo"));
}

#[tokio::test]
async fn reexport_module_doc() {
  let mod_doc_source_code = r#"
/**
 * This is some module doc.
 *
 * @module
 */

/** a variable */
export const a = "a";
"#;
  let ns_source_code = r#"
export * as b from "./mod_doc.ts";
"#;
  let (graph, analyzer, specifier) = setup(
    "file:///ns.ts",
    vec![
      ("file:///ns.ts", None, ns_source_code),
      ("file:///mod_doc.ts", None, mod_doc_source_code),
    ],
  )
  .await;
  let entries = DocParser::new(
    &graph,
    &analyzer,
    &[specifier],
    DocParserOptions::default(),
  )
  .unwrap()
  .parse()
  .unwrap()
  .into_values()
  .next()
  .unwrap();

  let actual = serde_json::to_value(&entries).unwrap();
  let expected = json!([
    {
      "kind": "namespace",
      "name": "b",
      "isDefault": false,
      "location": {
        "filename": "file:///ns.ts",
        "line": 2,
        "col": 7,
        "byteIndex": 8
      },
      "declarationKind": "export",
      "jsDoc": {
        "doc": "This is some module doc.\n",
        "tags": [
          {
            "kind": "module"
          }
        ]
      },
      "namespaceDef": {
        "elements": [
          {
            "kind": "variable",
            "name": "a",
            "isDefault": false,
            "location": {
              "filename": "file:///mod_doc.ts",
              "line": 9,
              "col": 13,
              "byteIndex": 83
            },
            "declarationKind": "export",
            "jsDoc": {
              "doc": "a variable"
            },
            "variableDef": {
              "tsType": {
                "repr": "a",
                "kind": "literal",
                "literal": {
                  "kind": "string",
                  "string": "a"
                }
              },
              "kind": "const"
            }
          }
        ]
      }
    }
  ]);
  assert_eq!(actual, expected);
}

#[tokio::test]
async fn filter_nodes_by_name() {
  use crate::find_nodes_by_name_recursively;
  let source_code = r#"
export namespace Deno {
  export class Buffer {}
  export function test(options: object): void;
  export function test(name: string, fn: Function): void;
  export function test(name: string | object, fn?: Function): void {}
}

export namespace Deno {
  export namespace Inner {
    export function a(): void {}
    export const b = 100;
  }

  export interface Conn {
    rid: number;
    closeWrite(): void;
  }

  export class Process {
    readonly pid: number;
    output(): Promise<Uint8Array>;
  }
}
"#;
  let (graph, analyzer, specifier) = setup(
    "file:///test.ts",
    vec![("file:///test.ts", None, source_code)],
  )
  .await;
  let entries = DocParser::new(
    &graph,
    &analyzer,
    &[specifier],
    DocParserOptions::default(),
  )
  .unwrap()
  .parse()
  .unwrap()
  .into_values()
  .next()
  .unwrap();

  // Namespace
  let found = find_nodes_by_name_recursively(entries.clone(), "Deno");
  assert_eq!(found.len(), 1);
  assert_eq!(found[0].name.as_ref(), "Deno");

  // Overloaded functions
  let found = find_nodes_by_name_recursively(entries.clone(), "Deno.test");
  assert_eq!(found.len(), 3);
  assert_eq!(found[0].name.as_ref(), "test");
  assert_eq!(found[1].name.as_ref(), "test");
  assert_eq!(found[2].name.as_ref(), "test");

  // Nested namespace
  let found = find_nodes_by_name_recursively(entries.clone(), "Deno.Inner.a");
  assert_eq!(found.len(), 1);
  assert_eq!(found[0].name.as_ref(), "a");

  // Interface property
  let found = find_nodes_by_name_recursively(entries.clone(), "Deno.Conn.rid");
  assert_eq!(found.len(), 1);
  assert_eq!(found[0].name.as_ref(), "rid");
  assert!(matches!(found[0].def, DocNodeDef::Variable { .. }));

  // Interface method
  let found =
    find_nodes_by_name_recursively(entries.clone(), "Deno.Conn.closeWrite");
  assert_eq!(found.len(), 1);
  assert_eq!(found[0].name.as_ref(), "closeWrite");
  assert!(matches!(found[0].def, DocNodeDef::Function { .. }));

  // Class property
  let found =
    find_nodes_by_name_recursively(entries.clone(), "Deno.Process.pid");
  assert_eq!(found.len(), 1);
  assert_eq!(found[0].name.as_ref(), "pid");
  assert!(matches!(found[0].def, DocNodeDef::Variable { .. }));

  // Class method
  let found =
    find_nodes_by_name_recursively(entries.clone(), "Deno.Process.output");
  assert_eq!(found.len(), 1);
  assert_eq!(found[0].name.as_ref(), "output");
  assert!(matches!(found[0].def, DocNodeDef::Function { .. }));

  // No match
  let found = find_nodes_by_name_recursively(entries.clone(), "Deno.test.a");
  assert_eq!(found.len(), 0);

  let found = find_nodes_by_name_recursively(entries, "a.b.c");
  assert_eq!(found.len(), 0);
}

#[tokio::test]
async fn exports_imported_earlier() {
  let foo_source_code = r#"export const foo: string = "foo";"#;
  let test_source_code = r#"
  import { foo } from "./foo.ts";

  export { foo };
  "#;

  let (graph, analyzer, specifier) = setup(
    "file:///test.ts",
    vec![
      ("file:///foo.ts", None, foo_source_code),
      ("file:///test.ts", None, test_source_code),
    ],
  )
  .await;
  let entries = DocParser::new(
    &graph,
    &analyzer,
    &[specifier],
    DocParserOptions::default(),
  )
  .unwrap()
  .parse()
  .unwrap()
  .into_values()
  .next()
  .unwrap();

  let expected_json = json!([
    {
      "kind": "variable",
      "name": "foo",
      "isDefault": false,
      "location": {
        "filename": "file:///foo.ts",
        "line": 1,
        "col": 13,
        "byteIndex": 13
      },
      "declarationKind": "export",
      "variableDef": {
        "tsType": {
          "repr": "string",
          "kind": "keyword",
          "keyword": "string"
        },
        "kind": "const"
      }
    },
    {
      "kind": "import",
      "name": "foo",
      "location": {
        "filename": "file:///test.ts",
        "line": 2,
        "col": 2,
        "byteIndex": 3
      },
      "declarationKind": "private",
      "importDef": {
        "src": "file:///foo.ts",
        "imported": "foo",
      },
    },
  ]);
  let actual = serde_json::to_value(&entries).unwrap();
  assert_eq!(actual, expected_json);
}

#[tokio::test]
async fn exports_imported_earlier_renamed() {
  let foo_source_code = r#"export const foo: string = "foo";"#;
  let test_source_code = r#"
  import { foo as f } from "./foo.ts";

  export { f };
  "#;

  let (graph, analyzer, specifier) = setup(
    "file:///test.ts",
    vec![
      ("file:///foo.ts", None, foo_source_code),
      ("file:///test.ts", None, test_source_code),
    ],
  )
  .await;
  let entries = DocParser::new(
    &graph,
    &analyzer,
    &[specifier],
    DocParserOptions::default(),
  )
  .unwrap()
  .parse()
  .unwrap()
  .into_values()
  .next()
  .unwrap();

  let expected_json = json!([
    {
      "kind": "variable",
      "name": "f",
      "isDefault": false,
      "location": {
        "filename": "file:///foo.ts",
        "line": 1,
        "col": 13,
        "byteIndex": 13
      },
      "declarationKind": "export",
      "variableDef": {
        "tsType": {
          "repr": "string",
          "kind": "keyword",
          "keyword": "string"
        },
        "kind": "const"
      }
    },
    {
      "kind": "import",
      "name": "f",
      "location": {
        "filename": "file:///test.ts",
        "line": 2,
        "col": 2,
        "byteIndex": 3
      },
      "declarationKind": "private",
      "importDef": {
        "src": "file:///foo.ts",
        "imported": "foo"
      }
    }
  ]);
  let actual = serde_json::to_value(&entries).unwrap();
  assert_eq!(actual, expected_json);
}

#[tokio::test]
async fn exports_imported_earlier_default() {
  let foo_source_code = r#"const foo: string = "foo";
  export default foo;"#;
  let test_source_code = r#"
  import foo from "./foo.ts";

  export { foo };
  "#;

  let (graph, analyzer, specifier) = setup(
    "file:///test.ts",
    vec![
      ("file:///foo.ts", None, foo_source_code),
      ("file:///test.ts", None, test_source_code),
    ],
  )
  .await;
  let entries = DocParser::new(
    &graph,
    &analyzer,
    &[specifier],
    DocParserOptions::default(),
  )
  .unwrap()
  .parse()
  .unwrap()
  .into_values()
  .next()
  .unwrap();

  let expected_json = json!([
    {
      "kind": "variable",
      "name": "foo",
      "isDefault": false,
      "location": {
        "filename": "file:///foo.ts",
        "line": 1,
        "col": 6,
        "byteIndex": 6
      },
      "declarationKind": "export",
      "variableDef": {
        "tsType": {
          "repr": "string",
          "kind": "keyword",
          "keyword": "string"
        },
        "kind": "const"
      }
    },
    {
      "kind": "import",
      "name": "foo",
      "location": {
        "filename": "file:///test.ts",
        "line": 2,
        "col": 2,
        "byteIndex": 3
      },
      "declarationKind": "private",
      "importDef": {
        "src": "file:///foo.ts",
        "imported": "default"
      }
    }
  ]);
  let actual = serde_json::to_value(&entries).unwrap();
  assert_eq!(actual, expected_json);
}

#[tokio::test]
async fn exports_imported_earlier_private() {
  let foo_source_code = r#"export const foo: string = "foo";"#;
  let test_source_code = r#"
  import { foo } from "./foo.ts";

  export { foo };
  "#;

  let (graph, analyzer, specifier) = setup(
    "file:///test.ts",
    vec![
      ("file:///foo.ts", None, foo_source_code),
      ("file:///test.ts", None, test_source_code),
    ],
  )
  .await;
  let entries = DocParser::new(
    &graph,
    &analyzer,
    &[specifier],
    DocParserOptions {
      private: true,
      ..Default::default()
    },
  )
  .unwrap()
  .parse()
  .unwrap()
  .into_values()
  .next()
  .unwrap();

  let expected_json = json!([
    {
      "kind": "variable",
      "name": "foo",
      "isDefault": false,
      "location": {
        "filename": "file:///foo.ts",
        "line": 1,
        "col": 13,
        "byteIndex": 13
      },
      "declarationKind": "export",
      "variableDef": {
        "tsType": {
          "repr": "string",
          "kind": "keyword",
          "keyword": "string"
        },
        "kind": "const"
      }
    },
    {
      "kind": "import",
      "name": "foo",
      "location": {
        "filename": "file:///test.ts",
        "line": 2,
        "col": 2,
        "byteIndex": 3
      },
      "declarationKind": "private",
      "importDef": {
        "src": "file:///foo.ts",
        "imported": "foo",
      },
    },
  ]);
  let actual = serde_json::to_value(&entries).unwrap();
  assert_eq!(actual, expected_json);
}

#[tokio::test]
async fn variable_syntax() {
  let (graph, analyzer, specifier) = setup(
    "file:///foo.ts",
    vec![
      ("file:///foo.ts", None, "export * from './bar.tsx'"),
      ("file:///bar.tsx", None, "export default <foo>bar</foo>"),
    ],
  )
  .await;

  // This just needs to not throw a syntax error
  DocParser::new(&graph, &analyzer, &[specifier], DocParserOptions::default())
    .unwrap()
    .parse()
    .unwrap();
}

#[tokio::test]
async fn json_module() {
  let (graph, analyzer, specifier) = setup(
    "file:///foo.ts",
    vec![
      ("file:///foo.ts", None, "export { default as configFile } from './bar.json' assert { type: 'json' };"),
      ("file:///bar.json", None, r#"{ "a": 5, "b": "text", "c": null, "d": [1, 2], "e": { "a": 1 } }"#),
    ],
  )
  .await;

  let entries = DocParser::new(
    &graph,
    &analyzer,
    &[specifier],
    DocParserOptions::default(),
  )
  .unwrap()
  .parse()
  .unwrap()
  .into_values()
  .next()
  .unwrap();

  let expected_json = json!([
    {
      "kind": "variable",
      "name": "configFile",
      "isDefault": false,
      "location": {
        "filename": "file:///bar.json",
        "line": 1,
        "col": 0,
        "byteIndex": 0,
      },
      "declarationKind": "export",
      "variableDef": {
        "tsType": {
          "repr": "",
          "kind": "typeLiteral",
          "typeLiteral": {
            "constructors": [],
            "methods": [],
            "properties": [{
              "name": "a",
              "location": {
                "filename": "",
                "line": 0,
                "col": 0,
                "byteIndex": 0,
              },
              "params": [],
              "computed": false,
              "optional": false,
              "tsType": {
                "repr": "5",
                "kind": "literal",
                "literal": {
                  "kind": "number",
                  "number": 5.0,
                },
              },
              "typeParams": []
            }, {
              "name": "b",
              "location": {
                "filename": "",
                "line": 0,
                "col": 0,
                "byteIndex": 0,
              },
              "params": [],
              "computed": false,
              "optional": false,
              "tsType": {
                "repr": "text",
                "kind": "literal",
                "literal": {
                  "kind": "string",
                  "string": "text",
                },
              },
              "typeParams": []
            }, {
              "name": "c",
              "location": {
                "filename": "",
                "line": 0,
                "col": 0,
                "byteIndex": 0,
              },
              "params": [],
              "computed": false,
              "optional": false,
              "tsType": {
                "repr": "null",
                "kind": "keyword",
                "keyword": "null",
              },
              "typeParams": []
            }, {
              "name": "d",
              "location": {
                "filename": "",
                "line": 0,
                "col": 0,
                "byteIndex": 0,
              },
              "params": [],
              "computed": false,
              "optional": false,
              "tsType": {
                "repr": "unknown[]",
                "kind": "array",
                "array": {
                  "repr": "unknown",
                  "kind": "keyword",
                  "keyword": "unknown",
                },
              },
              "typeParams": []
            }, {
              "name": "e",
              "params": [],
              "location": {
                "filename": "",
                "line": 0,
                "col": 0,
                "byteIndex": 0,
              },
              "computed": false,
              "optional": false,
              "tsType": {
                "repr": "",
                "kind": "typeLiteral",
                "typeLiteral": {
                  "constructors": [],
                  "methods": [],
                  "properties": [{
                    "name": "a",
                    "location": {
                      "filename": "",
                      "line": 0,
                      "col": 0,
                      "byteIndex": 0,
                    },
                    "params": [],
                    "computed": false,
                    "optional": false,
                    "tsType": {
                      "repr": "1",
                      "kind": "literal",
                      "literal": {
                        "kind": "number",
                        "number": 1.0,
                      },
                    },
                    "typeParams": []
                  }],
                  "callSignatures": [],
                  "indexSignatures": [],
                },
              },
              "typeParams": []
            }],
            "callSignatures": [],
            "indexSignatures": [],
          },
        },
        "kind": "var",
      },
    },
  ]);
  let actual = serde_json::to_value(&entries).unwrap();
  assert_eq!(actual, expected_json);
}

fn create_analyzer() -> CapturingModuleAnalyzer {
  CapturingModuleAnalyzer::default()
}

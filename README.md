# `deno_doc`

A Rust crate to generate documentation for JavaScript and TypeScript modules.

This crate powers
[`deno doc`](https://deno.land/manual/tools/documentation_generator), but is not
Deno specific and can be used to write documentation generators for other
targets like Node or the browser as well.

## Usage from Deno CLI or Deploy

This repostiroy includes a compiled version of the Rust crate as Web Assembly
and exposes an interface which is available via the `mod.ts` and can be imported
like this:

```ts
import * as denoDoc from "https://deno.land/x/deno_doc@{VERSION}/mod.ts";
```

Where `{VERSION}` should be substituted with the specific version you want to
use.

### `doc()`

The `doc()` function takes a string URL module specifier and potentially some
options, and asynchronously resolves with an array of documentation nodes, which
represent the surface API of the module.

A minimal example of using `doc()` and printing out some information about a
function:

```ts
import * as denoDoc from "https://deno.land/x/deno_doc@{VERSION}/mod.ts";

const colorsDoc = await doc("https://deno.land/std/fmt/colors.ts");

for (const node of colorsDoc) {
  console.log(`name: ${node.name} kind: ${node.kind}`);
}
```

The `doc()` function needs a way to retrieve modules, and by default uses a
`load()` function provided by `deno_graph` which uses `fetch()` for remote
modules and `Deno.readFile()` for local modules. This means that `doc()` will
require that appropriate read/net permissions to function properly. It will
prompt for them if not provided at startup.

### DocNode

The foundational type for the documentation is the `DocNode` and is exported
from the `mod.ts`.

## Rust Example

`examples/ddoc/main.rs` provides a minimal standalone binary demonstrating how
`deno_doc` can be used as a crate.

```shell
$ cargo run --example ddoc ../deno/std/http/mod.ts
```

## Developing

Make sure to have latest stable version of Rust installed (1.54.0).

```shell
// check version
$ rustc --version
rustc 1.54.0 (a178d0322 2021-07-26)

// build all targets
$ cargo build --all-targets

// test it
$ cargo test
```

## Contributing

- If you are going to work on an issue, mention so in the issue comments
  _before_ you start working on the issue.

- Please be professional in the forums. We follow
  [Rust's code of conduct](https://www.rust-lang.org/policies/code-of-conduct)
  (CoC) Have a problem? Email ry@tinyclouds.org.

- Ask for help in the [community chat room](https://discord.gg/deno).

## Submitting a Pull Request

Before submitting, please make sure the following is done:

1. That there is a related issue and it is referenced in the PR text.
2. There are tests that cover the changes.
3. Ensure `cargo test` passes.
4. Format your code with `rustfmt --check src/lib.rs`
5. Make sure `cargo clippy --all-targets --release --locked -- -D clippy::all`
   passes.

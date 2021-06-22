# `deno_doc`

A Rust crate to generate documentation for JavaScript and TypeScript modules.

This crate powers
[`deno doc`](https://deno.land/manual/tools/documentation_generator), but is not
Deno specific and can be used to write documentation generators for other
targets like Node or the browser as well.

## Example

`examples/ddoc/main.rs` provides a minimal standalone binary demonstrating how `deno_doc` can be used as a crate.

```shell
$ cargo run --example ddoc ../deno/std/http/mod.ts
```

## Developing

Make sure to have latest stable version of Rust installed (1.53.0).

```shell
// check version
$ rustc --version
rustc 1.53.0 (53cb7b09b 2021-06-17)

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

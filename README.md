# `deno_doc`

A Rust crate to generate documentation for JavaScript and TypeScript modules.

This crate powers
[`deno doc`](https://deno.land/manual/tools/documentation_generator), but is not
Deno specific and can be used to write documentation generators for other
targets like Node or the browser as well.

## Usage from Deno CLI or Deploy

See [js/README.md](js/README.md).

## Rust Example

`examples/ddoc/main.rs` provides a minimal standalone binary demonstrating how
`deno_doc` can be used as a crate.

```shell
$ cargo run --example ddoc ../deno_std/http/mod.ts
```

## Developing

```shell
# build all targets
$ cargo build --all-targets

# test it
$ cargo test

# build js api
$ deno task build

# test it
$ deno task test
```

### HTML generation

If you want to work on the HTML generation aspect of deno_doc, these things will
help you:

`deno task tailwind`: this regenerates the transpiled tailwind from the css
files and mentions of classes across the codebases, be it in rust files or js
files. This needs to always be run to do any updates to the styling.

`deno task gen_html`: This generates a `generated_docs` directory which is the
HTML output based on the provided files.

`deno task debug`: this calls the above tailwind task, and then the gen_html
task with all the files from `tests/testdata/multiple` passed.

We recommend to use these tasks above to develop features or debug things,
rather than recompiling a dependent on this system, as it is much faster
iteration and easier to debug.

We use [insta](https://github.com/mitsuhiko/insta) testing tool for taking
snapshots of the html output. If you change the rendering of html output, or
change the fixture files for html testing, you need to update snapshot using
[cargo-insta](https://insta.rs/docs/quickstart/) command.

```
# update snapshots
cargo insta test

# review snapshots
cargo insta review
```

See [the insta docs](https://insta.rs/docs/quickstart/) for more details.

## Contributing

- If you are going to work on an issue, mention so in the issue comments
  _before_ you start working on the issue.

- Please be professional in the forums. See our
  [Code of Conduct](https://github.com/denoland/deno/blob/main/.github/CODE_OF_CONDUCT.md).

- Ask for help in the [community chat room](https://discord.gg/deno).

## Submitting a Pull Request

Before submitting, please make sure the following is done:

1. That there is a related issue and it is referenced in the PR text.
2. There are tests that cover the changes.
3. Ensure `cargo test` and `deno task test` passes.
4. Format your code with `rustfmt --check src/lib.rs`
5. Make sure `cargo clippy --all-targets --release --locked -- -D clippy::all`
   passes.

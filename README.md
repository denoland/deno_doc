# `deno_doc`

Rust crate для создания документации для модулей JavaScript и TypeScript..

Этот crate крут
[`deno doc`](https://deno.land/manual/tools/documentation_generator), но не
специален для Deno и может быть использован для написания генератора
документации для чего то вроде node или браузера.

## Использование из Deno CLI или Deploy

Этот репозиторий включает скомпилированную версию Rust crate как Web Assembly и
предоставляет интерфейс, доступный через `mod.ts` и может быть импортирован так:

```ts
import { doc } from "https://deno.land/x/deno_doc@{VERSION}/mod.ts";
```

Где `{VERSION}` следует заменить на конкретную версию, которую вы хотите
использовать.

### `doc()`

Функция `doc()` принимает строку URL module specifier и, возможно, некоторые
параметры, и асинхронно разрешается с массивом узлов документации, которые
представляют поверхностный API модуля.

Минимальный пример использования `doc()` и печать некоторой информации о
функции:

```ts
import { doc } from "https://deno.land/x/deno_doc@{VERSION}/mod.ts";

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
$ cargo run --example ddoc ../deno_std/http/mod.ts
```

## Developing

Make sure to have latest stable version of Rust installed (1.55.0).

```shell
# check version
$ rustc --version
rustc 1.55.0 (c8dfcfe04 2021-09-06)

# build all targets
$ cargo build --all-targets

# test it
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

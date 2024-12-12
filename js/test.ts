// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

import { assert, assertEquals, assertRejects } from "jsr:@std/assert@0.223";
import { doc, generateHtml } from "./mod.ts";

Deno.test({
  name: "doc()",
  async fn() {
    const records = await doc(
      ["https://deno.land/std@0.104.0/fmt/colors.ts"],
    );
    const entries = Object.values(records)[0];
    assertEquals(entries.length, 49);
    const fnStripColor = entries.find((n) =>
      n.kind === "function" && n.name === "stripColor"
    );
    assert(fnStripColor, "unable to locate specific node");
    assert(fnStripColor.kind === "function");
    assert(fnStripColor.functionDef);
    assertEquals(fnStripColor.functionDef.params, [{
      kind: "identifier",
      name: "string",
      optional: false,
      tsType: {
        repr: "string",
        kind: "keyword",
        keyword: "string",
      },
    }]);
  },
});

Deno.test({
  name: "doc() - timings",
  async fn() {
    const fixture = [new URL("../benches/fixtures/deno.d.ts", import.meta.url)
      .toString()];

    const start = Date.now();
    await doc(fixture);
    const end = Date.now();
    console.log(`\n  First run: ${end - start}ms`);

    const runCount = 10;
    const time = [];

    for (let i = 0; i < runCount; i++) {
      const start = Date.now();
      await doc(fixture);
      const end = Date.now();
      time.push(end - start);
    }
    const totalTime = time.reduce((p, c) => p += c, 0);
    const meanTime = (totalTime / runCount).toFixed(0);
    console.log(`  Mean of ${runCount} runs: ${meanTime}ms`);
  },
});

Deno.test({
  name: "doc() - with headers",
  async fn() {
    const entries = await doc(["https://example.com/a"], {
      load(specifier) {
        return Promise.resolve({
          kind: "module",
          specifier,
          headers: {
            "content-type": "application/typescript; charset=utf-8",
          },
          content: `declare interface A {
            a: string;
          }`,
        });
      },
    });
    assertEquals(Object.values(entries)[0].length, 1);
  },
});

Deno.test({
  name: "doc() - bad specifier",
  async fn() {
    await assertRejects(
      async () => {
        await doc(["./bad.ts"]);
      },
      Error,
      "relative URL without a base",
    );
  },
});

Deno.test({
  name: "doc() - with import map",
  async fn() {
    const records = await doc(["https://example.com/a.ts"], {
      importMap: "https://example.com/import_map.json",
      load(specifier) {
        let content = "";
        switch (specifier) {
          case "https://example.com/import_map.json":
            content = JSON.stringify({
              imports: { "b": "https://example.com/b.ts" },
            });
            break;
          case "https://example.com/a.ts":
            content = `export { B } from "b";\n`;
            break;
          case "https://example.com/b.ts":
            content = `export class B {
              b: string;
            }`;
            break;
        }
        return Promise.resolve({
          kind: "module",
          specifier,
          content,
        });
      },
    });
    const entries = Object.values(records)[0];
    assertEquals(entries.length, 1);
    assertEquals(entries[0].kind, "class");
    assertEquals(entries[0].name, "B");
  },
});

Deno.test({
  name: "generateHtml()",
  async fn() {
    const entries = await doc(
      ["https://deno.land/std@0.104.0/fmt/colors.ts"],
    );

    const files = await generateHtml({
      ["file:///colors.ts"]: Object.values(entries)[0],
    }, {
      markdownRenderer(
        md,
        _titleOnly,
        _filePath,
        _anchorizer,
      ) {
        return md;
      },
      markdownStripper(md: string) {
        return md;
      },
    });

    assertEquals(Object.keys(files).length, 61);
  },
});

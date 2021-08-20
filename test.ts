// Copyright 2020-2021 the Deno authors. All rights reserved. MIT license.

import {
  assert,
  assertEquals,
  assertThrowsAsync,
} from "https://deno.land/std@0.104.0/testing/asserts.ts";
import { doc } from "./mod.ts";
import type { LoadResponse } from "./mod.ts";

Deno.test({
  name: "doc()",
  async fn() {
    const entries = await doc(
      "https://deno.land/std@0.104.0/fmt/colors.ts",
    );
    assertEquals(entries.length, 48);
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
    const fixture = new URL("./benches/fixtures/deno.d.ts", import.meta.url)
      .toString();

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
    const entries = await doc("https://example.com/a", {
      load(specifier) {
        return Promise.resolve({
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
    assertEquals(entries.length, 1);
  },
});

Deno.test({
  name: "doc() - missing specifier",
  // TODO(@kitsonk) - remove when new deno_graph crate published
  sanitizeResources: false,
  fn() {
    return assertThrowsAsync(
      async () => {
        await doc("https://deno.land/x/bad.ts");
      },
      Error,
      `Unable to load specifier: "https://deno.land/x/bad.ts"`,
    );
  },
});

Deno.test({
  name: "doc() - bad specifier",
  fn() {
    return assertThrowsAsync(
      async () => {
        await doc("./bad.ts");
      },
      Error,
      "relative URL without a base",
    );
  },
});

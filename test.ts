// Copyright 2020-2021 the Deno authors. All rights reserved. MIT license.

import {
  assert,
  assertEquals,
} from "https://deno.land/std@0.104.0/testing/asserts.ts";
import { doc } from "./mod.ts";

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

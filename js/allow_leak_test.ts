// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

import { assertRejects } from "jsr:@std/assert@0.223";
import { doc } from "./mod.ts";

Deno.test({
  name: "doc() - missing specifier",
  // TODO(@kitsonk) - remove when new deno_graph crate published
  sanitizeResources: false,
  async fn() {
    await assertRejects(
      async () => {
        await doc("https://deno.land/x/bad.ts");
      },
      Error,
      `Module not found "https://deno.land/x/bad.ts".`,
    );
  },
});

// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

import { assertRejects } from "@std/assert";
import { doc } from "./mod.ts";

Deno.test({
  name: "doc() - missing specifier",
  async fn() {
    await assertRejects(
      async () => {
        await doc(["https://deno.land/x/bad.ts"]);
      },
      Error,
      `Module not found "https://deno.land/x/bad.ts".`,
    );
  },
});

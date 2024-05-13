// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

import { instantiate } from "./deno_doc_wasm.generated.js";
import type { DocNode } from "./types.d.ts";
import { createCache } from "jsr:@deno/cache-dir@0.8";
import type { CacheSetting, LoadResponse } from "jsr:@deno/graph@0.73";

export type { CacheSetting, LoadResponse } from "jsr:@deno/graph@0.73";
export * from "./types.d.ts";

const encoder = new TextEncoder();

export interface DocOptions {
  /** An optional URL string which provides a location of an import map to be
   * loaded and used to resolve module specifiers. This should be an absolute
   * value.
   *
   * When a `resolve()` function is also specified, a warning will be issued
   * and the import map will be used instead of the `resolve()` function. */
  importMap?: string;
  /** Print import map diagnostics.
   *
   * @default {true}
   */
  printImportMapDiagnostics?: boolean;
  /** If `true` include all documentation nodes in the output, included private
   * (non-exported) nodes. The default is `false`.  Use the `declarationKind`
   * of the `DocNode` to determine if the doc node is private, exported,
   * imported, or declared. */
  includeAll?: boolean;
  /**
   * An optional callback that is called with the URL string of the resource to
   * be loaded and a flag indicating if the module was required dynamically. The
   * callback should resolve with a `LoadResponse` or `undefined` if the module
   * is not found. If there are other errors encountered, a rejected promise
   * should be returned.
   *
   * This defaults to a load function which will use `fetch()` and
   * `Deno.readFile()` to load modules, and requires the appropriate permissions
   * to function. If the permissions are note available at startup, the default
   * function will prompt for them.
   *
   * @param specifier The URL string of the resource to be loaded and resolved
   * @param isDynamic A flag that indicates if the module was being loaded
   *                  dynamically
   */
  load?(
    specifier: string,
    isDynamic?: boolean,
    cacheSetting?: CacheSetting,
    checksum?: string,
  ): Promise<LoadResponse | undefined>;
  /** An optional callback that allows the default resolution logic of the
   * module graph to be "overridden". This is intended to allow items like an
   * import map to be used with the module graph. The callback takes the string
   * of the module specifier from the referrer and the string URL of the
   * referrer. The callback then returns a resolved URL string specifier.
   *
   * When an `importMap` URL string and this method is specifier, a warning
   * will be issued and the import map will be used. */
  resolve?(specifier: string, referrer: string): string;
}

/**
 * Generate asynchronously an array of documentation nodes for the supplied
 * module.
 *
 * ### Example
 *
 * ```ts
 * import { doc } from "https://deno.land/x/deno_doc/mod.ts";
 *
 * const entries = await doc("https://deno.land/std/fmt/colors.ts");
 *
 * for (const entry of entries) {
 *   console.log(`name: ${entry.name} kind: ${entry.kind}`);
 * }
 * ```
 *
 * @param specifier The URL string of the specifier to document
 * @param options A set of options for generating the documentation
 * @returns A promise that resolves with an array of documentation nodes
 */
export async function doc(
  specifier: string,
  options: DocOptions = {},
): Promise<Array<DocNode>> {
  const {
    load = createCache().load,
    includeAll = false,
    resolve,
    importMap,
    printImportMapDiagnostics = true,
  } = options;

  const wasm = await instantiate();
  return wasm.doc(
    specifier,
    includeAll,
    (specifier: string, options: {
      isDynamic: boolean;
      cacheSetting: CacheSetting;
      checksum: string | undefined;
    }) => {
      return load(
        specifier,
        options.isDynamic,
        options.cacheSetting,
        options.checksum,
      ).then((result) => {
        if (result?.kind === "module") {
          if (typeof result.content === "string") {
            result.content = encoder.encode(result.content);
          }
          // need to convert to an array for serde_wasm_bindgen to work
          // deno-lint-ignore no-explicit-any
          (result as any).content = Array.from(result.content);
        }
        return result;
      });
    },
    resolve,
    importMap,
    printImportMapDiagnostics,
  );
}

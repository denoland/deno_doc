// Copyright 2020-2021 the Deno authors. All rights reserved. MIT license.

import { doc as jsDoc } from "./lib/deno_doc.js";
import type { DocNode } from "./lib/types.d.ts";
import { load as defaultLoad } from "https://deno.land/x/deno_graph@0.1.0/lib/loader.ts";
import type { LoadResponse } from "https://deno.land/x/deno_graph@0.1.0/mod.ts";

export type {
  DocNode,
  DocNodeClass,
  DocNodeEnum,
  DocNodeFunction,
  DocNodeImport,
  DocNodeInterface,
  DocNodeNamespace,
  DocNodeTypeAlias,
  DocNodeVariable,
} from "./lib/types.d.ts";

export interface DocOptions {
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
    isDynamic: boolean,
  ): Promise<LoadResponse | undefined>;
  /** An optional callback that allows the default resolution logic of the
   * module graph to be "overridden". This is intended to allow items like an
   * import map to be used with the module graph. The callback takes the string
   * of the module specifier from the referrer and the string URL of the
   * referrer. The callback then returns a resolved URL string specifier. */
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
export function doc(
  specifier: string,
  options: DocOptions = {},
): Promise<Array<DocNode>> {
  const { load = defaultLoad, resolve } = options;
  return jsDoc(specifier, load, resolve);
}

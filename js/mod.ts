// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

import { instantiate } from "./deno_doc_wasm.generated.js";
import type { DocNode, Location } from "./types.d.ts";
import type { Page } from "./html.d.ts";
import { createCache } from "@deno/cache-dir";
import type { CacheSetting, LoadResponse } from "@deno/graph";

export type { CacheSetting, LoadResponse } from "@deno/graph";
export * from "./types.d.ts";
export * from "./html.d.ts";

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
 * const entries = await doc(["https://deno.land/std/fmt/colors.ts"]);
 *
 * for (const entry of entries) {
 *   console.log(`name: ${entry.name} kind: ${entry.kind}`);
 * }
 * ```
 *
 * @param specifiers List of the URL strings of the specifiers to document
 * @param options A set of options for generating the documentation
 * @returns A promise that resolves with an array of documentation nodes
 */
export async function doc(
  specifiers: string[],
  options: DocOptions = {},
): Promise<Record<string, Array<DocNode>>> {
  const {
    load = createCache().load,
    includeAll = false,
    resolve,
    importMap,
    printImportMapDiagnostics = true,
  } = options;

  const wasm = await instantiate();
  return wasm.doc(
    specifiers,
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

export interface ShortPath {
  /** Name identifier for the path. */
  path: string;
  /** URL for the path. */
  specifier: string;
  /** Whether the path is the main entrypoint. */
  isMain: boolean;
}

export interface UrlResolveKindRoot {
  kind: "root";
}

export interface UrlResolveKindAllSymbols {
  kind: "allSymbols";
}

export interface UrlResolveKindCategory {
  kind: "category";
  category: string;
}

export interface UrlResolveKindFile {
  kind: "file";
  file: ShortPath;
}

export interface UrlResolveKindSymbol {
  kind: "symbol";
  file: ShortPath;
  symbol: string;
}

export type UrlResolveKind =
  | UrlResolveKindRoot
  | UrlResolveKindAllSymbols
  | UrlResolveKindCategory
  | UrlResolveKindFile
  | UrlResolveKindSymbol;

export interface HrefResolver {
  /** Resolver for how files should link to each other. */
  resolvePath?(
    current: UrlResolveKind,
    target: UrlResolveKind,
    defaultResolve: () => string,
  ): string;
  /** Resolver for global symbols, like the Deno namespace or other built-ins */
  resolveGlobalSymbol?(symbol: string[]): string | undefined;
  /** Resolver for symbols from non-relative imports */
  resolveImportHref?(symbol: string[], src: string): string | undefined;
  /** Resolve the URL used in source code link buttons. */
  resolveSource?(location: Location): string | undefined;
  /**
   * Resolve external JSDoc module links.
   * Returns a tuple with link and title.
   */
  resolveExternalJsdocModule?(
    module: string,
    symbol?: string,
  ): { link: string; title: string } | undefined;
}

export interface UsageComposerEntry {
  /** Name for the entry. Can be left blank in singleMode. */
  name: string;
  /** Icon for the entry. */
  icon?: string;
}

export type UsageToMd = (
  url: string,
  customFileIdentifier: string | undefined,
) => string;

export interface UsageComposer {
  /** Whether the usage should only display a single item and not have a dropdown. */
  singleMode: boolean;

  /**
   * Composer to generate usage.
   *
   * @param currentResolve The current resolve.
   * @param usageToMd Callback to generate a usage import block.
   */
  compose(
    currentResolve: UrlResolveKind,
    usageToMd: UsageToMd,
  ): Map<UsageComposerEntry, string>;
}

export interface GenerateOptions {
  /** The name of the package to use in the breadcrumbs. */
  packageName?: string;
  /** The main entrypoint if one is present. */
  mainEntrypoint?: string;
  /** Composer for generating the usage of a symbol of module. */
  usageComposer?: UsageComposer;
  /** Resolver for how links should be resolved. */
  hrefResolver?: HrefResolver;
  /** Map for remapping module names to a custom value. */
  rewriteMap?: Record<string, string>;
  /**
   * Map of categories to their markdown description.
   * Only usable in category mode (single d.ts file with categories declared).
   */
  categoryDocs?: Record<string, string | undefined>;
  /** Whether to disable search. */
  disableSearch?: boolean;
  /**
   * Map of modules, where the value is a map of symbols with value of a link to
   * where this symbol should redirect to.
   */
  symbolRedirectMap?: Record<string, Record<string, string>>;
  /**
   * Map of modules, where the value is what the name of the default symbol should be.
   */
  defaultSymbolMap?: Record<string, string>;
  /**
   * Hook to inject content in the `head` tag.
   *
   * @param root the path to the root of the output.
   */
  headInject?(root: string): string;
  /**
   * Function to render markdown.
   *
   * @param md The raw markdown that needs to be rendered.
   * @param titleOnly Whether only the title should be rendered. Recommended syntax to keep is:
   * - paragraph
   * - heading
   * - text
   * - code
   * - html inline
   * - emph
   * - strong
   * - strikethrough
   * - superscript
   * - link
   * - math
   * - escaped
   * - wiki link
   * - underline
   * - soft break
   * @param filePath The filepath where the rendering is happening.
   * @param anchorizer Anchorizer used to generate slugs and the sidebar.
   * @return The rendered markdown.
   */
  markdownRenderer(
    md: string,
    titleOnly: boolean,
    filePath: ShortPath | undefined,
    anchorizer: (content: string, depthLevel: number) => string,
  ): string | undefined;
  /** Function to strip markdown. */
  markdownStripper(md: string): string;
}

const defaultUsageComposer: UsageComposer = {
  singleMode: true,
  compose(currentResolve, usageToMd) {
    if ("file" in currentResolve) {
      return new Map([[
        { name: "" },
        usageToMd(currentResolve.file.specifier, undefined),
      ]]);
    } else {
      return new Map();
    }
  },
};

/**
 * Generate HTML files for provided {@linkcode DocNode}s.
 * @param docNodesByUrl DocNodes keyed by their absolute URL.
 * @param options Options for the generation.
 */
export async function generateHtml(
  docNodesByUrl: Record<string, Array<DocNode>>,
  options: GenerateOptions,
): Promise<Record<string, string>> {
  const {
    usageComposer = defaultUsageComposer,
  } = options;

  const wasm = await instantiate();
  return wasm.generate_html(
    options.packageName,
    options.mainEntrypoint,
    usageComposer.singleMode,
    usageComposer.compose,
    options.rewriteMap,
    options.categoryDocs,
    options.disableSearch ?? false,
    options.symbolRedirectMap,
    options.defaultSymbolMap,
    options.hrefResolver?.resolvePath,
    options.hrefResolver?.resolveGlobalSymbol || (() => undefined),
    options.hrefResolver?.resolveImportHref || (() => undefined),
    options.hrefResolver?.resolveSource || (() => undefined),
    options.hrefResolver?.resolveExternalJsdocModule || (() => undefined),
    options.markdownRenderer,
    options.markdownStripper,
    options.headInject,
    docNodesByUrl,
    false,
  );
}

/**
 * Generate JSON data equivalent to the HTML files generated by {@linkcode generateHtml}.
 * @param docNodesByUrl DocNodes keyed by their absolute URL.
 * @param options Options for the generation.
 */
export async function generateHtmlAsJSON(
  docNodesByUrl: Record<string, Array<DocNode>>,
  options: GenerateOptions,
): Promise<Record<string, Page>> {
  const {
    usageComposer = defaultUsageComposer,
  } = options;

  const wasm = await instantiate();
  return wasm.generate_html(
    options.packageName,
    options.mainEntrypoint,
    usageComposer.singleMode,
    usageComposer.compose,
    options.rewriteMap,
    options.categoryDocs,
    options.disableSearch ?? false,
    options.symbolRedirectMap,
    options.defaultSymbolMap,
    options.hrefResolver?.resolvePath,
    options.hrefResolver?.resolveGlobalSymbol || (() => undefined),
    options.hrefResolver?.resolveImportHref || (() => undefined),
    options.hrefResolver?.resolveSource || (() => undefined),
    options.hrefResolver?.resolveExternalJsdocModule || (() => undefined),
    options.markdownRenderer,
    options.markdownStripper,
    options.headInject,
    docNodesByUrl,
    true,
  );
}

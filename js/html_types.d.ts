// Copyright 2018-2025 the Deno authors. All rights reserved. MIT license.

export interface HtmlHeadCtx {
  title: string;
  current_file: string;
  stylesheet_url: string;
  page_stylesheet_url: string;
  reset_stylesheet_url: string;
  url_search_index: string;
  script_js: string;
  fuse_js: string;
  url_search: string;
  head_inject: string | null;
  disable_search: boolean;
}

export interface CategoriesPanelCtx {
  categories: CategoriesPanelCategoryCtx[];
  all_symbols_href: string;
  total_symbols: number;
}

export interface CategoriesPanelCategoryCtx {
  name: string;
  href: string;
  active: boolean;
}

export type Page =
  | IndexCtx
  | AllSymbolsPageCtx
  | SymbolPageCtx
  | Redirect
  | Search;

export interface PageBase {
  kind: "IndexCtx" | "AllSymbolsPageCtx" | "SymbolPageCtx";
  html_head_ctx: HtmlHeadCtx;
  disable_search: boolean;
  categories_panel: CategoriesPanelCtx | null;
  breadcrumbs_ctx: BreadcrumbsCtx;
}

export interface IndexCtx extends PageBase {
  kind: "IndexCtx";
  overview: SymbolContentCtx | null;
  module_doc: ModuleDocCtx | null;
  usage: UsagesCtx | null;
  toc_ctx: ToCCtx;
}

export interface AllSymbolsPageCtx extends PageBase {
  kind: "AllSymbolsPageCtx";
  content: AllSymbolsCtx;
}

export interface AllSymbolsCtx {
  entrypoints: AllSymbolsItemCtx[];
}

export interface AllSymbolsItemCtx {
  name: string;
  href: string;
  module_doc: ModuleDocCtx;
}

export interface SymbolPageCtx extends PageBase {
  kind: "SymbolPageCtx";
  symbol_group_ctx: SymbolGroupCtx;
  toc_ctx: ToCCtx;
}

export interface Redirect {
  kind: "redirect";
  path: string;
}

export interface Search {
  kind: "search";
  path: SearchIndexNode[];
}

export interface SlimKindCtx {
  char: string;
  kind: string;
  title: string;
}

export interface SearchIndexNode {
  kind: SlimKindCtx[];
  name: string;
  file: string;
  doc: string;
  url: string;
  category?: string;
  deprecated: boolean;
}

export interface ToCCtx {
  usages: UsagesCtx | null;
  top_symbols: TopSymbolsCtx | null;
  document_navigation_str: string | null;
  document_navigation: ToCEntry[];
}

export interface ToCEntry {
  level: number;
  content: string;
  anchor: string;
}

export interface UsagesCtx {
  usages: UsageCtx[];
  composed: boolean;
}

export interface UsageCtx {
  name: string;
  content: string;
  icon: string | null;
  additional_css: string;
}

export interface BreadcrumbsCtx {
  root: BreadcrumbCtx;
  current_entrypoint: BreadcrumbCtx | null;
  entrypoints: BreadcrumbCtx[];
  symbol: BreadcrumbCtx[];
}

export interface BreadcrumbCtx {
  name: string;
  href: string;
}

export interface TopSymbolsCtx {
  symbols: TopSymbolCtx[];
  total_symbols: number;
  all_symbols_href: string;
}

export interface TopSymbolCtx {
  kind: DocNodeKindCtx[];
  name: string;
  href: string;
}

export interface ModuleDocCtx {
  deprecated: string | null;
  sections: SymbolContentCtx;
}

export interface DocNodeKindCtx {
  kind: string;
  char: string;
  title: string;
  title_lowercase: string;
  title_plural: string;
}

export interface SymbolContentCtx {
  id: string;
  docs: string | null;
  sections: SectionCtx[];
}

export interface SectionCtx {
  header: SectionHeaderCtx | null;
  content: SectionContentCtx;
}

export interface SectionHeaderCtx {
  title: string;
  anchor: AnchorCtx;
  href: string | null;
  doc: string | null;
}

export interface AnchorCtx {
  id: string;
}

export interface SymbolGroupCtx {
  name: string;
  symbols: SymbolCtx[];
}

export interface SymbolCtx {
  kind: DocNodeKindCtx;
  usage: UsagesCtx | null;
  tags: Tag[];
  subtitle: DocBlockSubtitleCtx | null;
  content: SymbolInnerCtx[];
  deprecated: string | null;
  source_href: string | null;
}

export type DocBlockSubtitleCtx =
  | DocBlockSubtitleClassCtx
  | DocBlockSubtitleInterfaceCtx;

export interface DocBlockSubtitleClassCtx {
  kind: "class";
  value: DocBlockSubtitleClassValueCtx;
}
export interface DocBlockSubtitleClassValueCtx {
  implements: string[] | null;
  extends: DocBlockClassSubtitleExtendsCtx | null;
}

export interface DocBlockClassSubtitleExtendsCtx {
  href: string | null;
  symbol: string;
  type_args: string;
}

export interface DocBlockSubtitleInterfaceCtx {
  kind: "interface";
  value: DocBlockSubtitleInterfaceValueCtx;
}

export interface DocBlockSubtitleInterfaceValueCtx {
  extends: string[];
}

export type SymbolInnerCtx = SymbolInnerFunctionCtx | SymbolInnerOtherCtx;

export interface SymbolInnerFunctionCtx {
  kind: "function";
  value: FunctionCtx;
}

export interface SymbolInnerOtherCtx {
  kind: "other";
  value: SymbolContentCtx;
}

export interface FunctionCtx {
  functions: OverloadRenderCtx[];
}

export interface OverloadRenderCtx {
  id: string;
  anchor: AnchorCtx;
  name: string;
  summary: string;
  deprecated: string | null;
  content: SymbolContentCtx;
}

export type SectionContentCtx =
  | SectionContentDocEntryCtx
  | SectionContentExampleCtx
  | SectionContentIndexSignatureCtx
  | SectionContentNamespaceSectionCtx
  | SectionContentNamespaceSeeCtx
  | SectionContentEmptyCtx;

export interface SectionContentDocEntryCtx {
  kind: "doc_entry";
  content: DocEntryCtx[];
}

export interface SectionContentExampleCtx {
  kind: "example";
  content: ExampleCtx[];
}

export interface SectionContentIndexSignatureCtx {
  kind: "index_signature";
  content: IndexSignatureCtx[];
}

export interface SectionContentNamespaceSectionCtx {
  kind: "namespace_section";
  content: NamespaceNodeCtx[];
}

export interface SectionContentNamespaceSeeCtx {
  kind: "see";
  content: string[];
}

export interface SectionContentEmptyCtx {
  kind: "empty";
}

export interface DocEntryCtx {
  id: string;
  name: string | null;
  name_href: string | null;
  content: string;
  anchor: AnchorCtx;
  tags: Tag[];
  js_doc: string | null;
  source_href: string | null;
}

export interface ExampleCtx {
  anchor: AnchorCtx;
  id: string;
  title: string;
  markdown_title: string;
  markdown_body: string;
}

export interface IndexSignatureCtx {
  id: string;
  anchor: AnchorCtx;
  readonly: boolean;
  params: string;
  ts_type: string;
  source_href: string | null;
}

export interface NamespaceNodeCtx {
  id: string;
  anchor: AnchorCtx;
  tags: Tag[];
  doc_node_kind_ctx: DocNodeKindCtx[];
  href: string;
  name: string;
  ty: string | null;
  docs: string | null;
  deprecated: boolean;
  subitems: NamespaceNodeSubItemCtx[];
}

export interface NamespaceNodeSubItemCtx {
  title: string;
  docs: string | null;
  ty: string | null;
  href: string;
}

export type Tag =
  | TagNew
  | TagAbstract
  | TagDeprecated
  | TagWriteonly
  | TagReadonly
  | TagProtected
  | TagPrivate
  | TagOptional
  | TagUnstable
  | TagPermissions
  | TagOther;

export interface TagNew {
  kind: "new";
}

export interface TagAbstract {
  kind: "abstract";
}

export interface TagDeprecated {
  kind: "deprecated";
}

export interface TagWriteonly {
  kind: "writeonly";
}

export interface TagReadonly {
  kind: "readonly";
}

export interface TagProtected {
  kind: "protected";
}

export interface TagPrivate {
  kind: "private";
}

export interface TagOptional {
  kind: "optional";
}

export interface TagUnstable {
  kind: "unstable";
}

export interface TagPermissions {
  kind: "permissions";
  value: string[];
}

export interface TagOther {
  kind: "other";
  value: string;
}

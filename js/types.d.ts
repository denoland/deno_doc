// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

export interface Document {
  moduleDoc?: JsDoc;
  imports?: Import[];
  symbols: DocNode[];
}

export interface Import {
  importedName: string;
  aliasedName?: string;
  location: Location;
  jsDoc?: JsDoc;
  src: string;
  imported?: string;
}

export interface DocNode {
  name: string;
  isDefault?: true;
  declarations: Declaration[];
}

/** Indicates how the documentation node was declared. `"private"` indicates
 * the node is un-exported. `"export"` indicates it is exported from the current
 * module. `"declare"` indicates that it is a type only declaration. */
export type DeclarationKind = "private" | "export" | "declare";

export type Declaration =
  | DeclarationModuleDoc
  | DeclarationFunction
  | DeclarationVariable
  | DeclarationEnum
  | DeclarationClass
  | DeclarationTypeAlias
  | DeclarationNamespace
  | DeclarationInterface
  | DeclarationReference;

interface DeclarationBase {
  kind: DocNodeKind;
  location: Location;
  declarationKind: DeclarationKind;
  jsDoc?: JsDoc;
}

export type DocNodeKind =
  | "moduleDoc"
  | "function"
  | "variable"
  | "enum"
  | "class"
  | "typeAlias"
  | "namespace"
  | "interface"
  | "reference";

export interface DeclarationModuleDoc extends DeclarationBase {
  kind: "moduleDoc";
  jsDoc: JsDoc;
}

export interface DeclarationFunction extends DeclarationBase {
  kind: "function";
  def: FunctionDef;
}

export interface DeclarationVariable extends DeclarationBase {
  kind: "variable";
  def: VariableDef;
}

export interface DeclarationEnum extends DeclarationBase {
  kind: "enum";
  enumDef: EnumDef;
}

export interface DeclarationClass extends DeclarationBase {
  kind: "class";
  def: ClassDef;
}

export interface DeclarationTypeAlias extends DeclarationBase {
  kind: "typeAlias";
  def: TypeAliasDef;
}

export interface DeclarationNamespace extends DeclarationBase {
  kind: "namespace";
  def: NamespaceDef;
}

export interface DeclarationInterface extends DeclarationBase {
  kind: "interface";
  def: InterfaceDef;
}

export interface DeclarationReference extends DeclarationBase {
  kind: "reference";
  reference_def: ReferenceDef;
}

export type Accessibility = "public" | "protected" | "private";

export interface ClassDef {
  defName?: string;
  isAbstract: boolean;
  constructors: ClassConstructorDef[];
  properties: ClassPropertyDef[];
  indexSignatures: ClassIndexSignatureDef[];
  methods: ClassMethodDef[];
  extends?: string;
  implements: TsTypeDef[];
  typeParams: TsTypeParamDef[];
  superTypeParams: TsTypeDef[];
  decorators?: DecoratorDef[];
}

export type ClassConstructorParamDef = ParamDef & {
  accessibility?: Accessibility;
  isOverride?: boolean;
  readonly?: boolean;
};

export interface ClassConstructorDef {
  jsDoc?: JsDoc;
  accessibility?: Accessibility;
  isOptional?: boolean;
  hasBody?: boolean;
  name: string;
  params: ClassConstructorParamDef[];
  location: Location;
}

export interface ClassIndexSignatureDef {
  readonly: boolean;
  params: ParamDef[];
  tsType?: TsTypeDef;
}

export interface ClassMethodDef {
  jsDoc?: JsDoc;
  accessibility?: Accessibility;
  optional: boolean;
  isAbstract: boolean;
  isStatic: boolean;
  isOverride?: boolean;
  name: string;
  kind: MethodKind;
  def: FunctionDef;
  location: Location;
}

export interface ClassPropertyDef {
  jsDoc?: JsDoc;
  tsType?: TsTypeDef;
  readonly: boolean;
  accessibility?: Accessibility;
  optional: boolean;
  isAbstract: boolean;
  isStatic: boolean;
  isOverride?: boolean;
  name: string;
  decorators?: DecoratorDef[];
  location: Location;
}

export interface DecoratorDef {
  name: string;
  args?: string[];
  location: Location;
}

export interface EnumDef {
  members: EnumMemberDef[];
}

export interface EnumMemberDef {
  name: string;
  init?: TsTypeDef;
  jsDoc?: JsDoc;
  location: Location;
}

export interface FunctionDef {
  defName?: string;
  params: ParamDef[];
  returnType?: TsTypeDef;
  hasBody?: boolean;
  isAsync: boolean;
  isGenerator: boolean;
  typeParams: TsTypeParamDef[];
  decorators?: DecoratorDef[];
}

export interface InterfaceDef {
  defName?: string;
  extends: TsTypeDef[];
  methods: InterfaceMethodDef[];
  properties: InterfacePropertyDef[];
  callSignatures: InterfaceCallSignatureDef[];
  indexSignatures: InterfaceIndexSignatureDef[];
  typeParams: TsTypeParamDef[];
  constructors?: unknown[];
}

export interface InterfaceCallSignatureDef {
  location: Location;
  jsDoc?: JsDoc;
  params: ParamDef[];
  tsType?: TsTypeDef;
  typeParams: TsTypeParamDef[];
}

export interface InterfaceIndexSignatureDef {
  readonly: boolean;
  params: ParamDef[];
  tsType?: TsTypeDef;
}

export interface InterfaceMethodDef {
  name: string;
  kind: MethodKind;
  location: Location;
  jsDoc?: JsDoc;
  computed?: boolean;
  optional: boolean;
  params: ParamDef[];
  returnType?: TsTypeDef;
  typeParams: TsTypeParamDef[];
}

export interface InterfacePropertyDef {
  name: string;
  location: Location;
  jsDoc?: JsDoc;
  params: ParamDef[];
  readonly?: boolean;
  computed: boolean;
  optional: boolean;
  tsType?: TsTypeDef;
  typeParams: TsTypeParamDef[];
}

export interface ReferenceDef {
  target: Location;
}

export interface JsDoc {
  doc?: string;
  tags?: JsDocTag[];
}

export type JsDocTagKind =
  | "callback"
  | "category"
  | "constructor"
  | "default"
  | "deprecated"
  | "enum"
  | "example"
  | "extends"
  | "ignore"
  | "module"
  | "param"
  | "public"
  | "private"
  | "property"
  | "protected"
  | "readonly"
  | "return"
  | "tags"
  | "template"
  | "this"
  | "typedef"
  | "type"
  | "see"
  | "throws"
  | "unsupported";

export type JsDocTag =
  | JsDocTagOnly
  | JsDocTagDoc
  | JsDocTagDocRequired
  | JsDocTagNamed
  | JsDocTagValued
  | JsDocTagTyped
  | JsDocTagNamedTyped
  | JsDocTagParam
  | JsDocTagReturn
  | JsDocTagModule
  | JsDocTagThrows
  | JsDocTagTags
  | JsDocTagUnsupported;

export interface JsDocTagBase {
  kind: JsDocTagKind;
}

export interface JsDocTagOnly extends JsDocTagBase {
  kind:
    | "constructor"
    | "ignore"
    | "public"
    | "private"
    | "protected"
    | "readonly";
}

export interface JsDocTagDoc extends JsDocTagBase {
  kind: "deprecated";
  doc?: string;
}

export interface JsDocTagDocRequired extends JsDocTagBase {
  kind: "category" | "example" | "see";
  doc: string;
}

export interface JsDocTagNamed extends JsDocTagBase {
  kind: "callback" | "template";
  name: string;
  doc?: string;
}

export interface JsDocTagValued extends JsDocTagBase {
  kind: "default";
  value: string;
  doc?: string;
}

export interface JsDocTagTyped extends JsDocTagBase {
  kind: "enum" | "extends" | "this" | "type";
  type: string;
  doc?: string;
}

export interface JsDocTagNamedTyped extends JsDocTagBase {
  kind: "property" | "typedef";
  name: string;
  type: string;
  doc?: string;
}

export interface JsDocTagParam extends JsDocTagBase {
  kind: "param";
  name: string;
  type?: string;
  optional?: true;
  default?: string;
  doc?: string;
}

export interface JsDocTagReturn extends JsDocTagBase {
  kind: "return";
  type?: string;
  doc?: string;
}

export interface JsDocTagThrows extends JsDocTagBase {
  kind: "throws";
  type?: string;
  doc: string;
}

export interface JsDocTagModule extends JsDocTagBase {
  kind: "module";
  name?: string;
}

export interface JsDocTagTags extends JsDocTagBase {
  kind: "tags";
  tags: string[];
}

export interface JsDocTagUnsupported extends JsDocTagBase {
  kind: "unsupported";
  value: string;
}

export interface LiteralCallSignatureDef {
  params: ParamDef[];
  tsType?: TsTypeDef;
  typeParams: TsTypeParamDef[];
}

export type LiteralDefKind =
  | "number"
  | "string"
  | "template"
  | "boolean"
  | "bigInt";

export type LiteralDef =
  | LiteralDefNumber
  | LiteralDefBigInt
  | LiteralDefString
  | LiteralDefTemplate
  | LiteralDefBoolean;

interface LiteralDefBase {
  kind: LiteralDefKind;
}

export interface LiteralDefNumber extends LiteralDefBase {
  kind: "number";
  number: number;
}

export interface LiteralDefBigInt extends LiteralDefBase {
  kind: "bigInt";
  string: string;
}

export interface LiteralDefString extends LiteralDefBase {
  kind: "string";
  string: string;
}

export interface LiteralDefTemplate extends LiteralDefBase {
  kind: "template";
  tsTypes: TsTypeDef[];
}

export interface LiteralDefBoolean extends LiteralDefBase {
  kind: "boolean";
  boolean: boolean;
}

export interface LiteralIndexSignatureDef {
  readonly: boolean;
  params: ParamDef[];
  tsType?: TsTypeDef;
  location?: Location;
}

export interface LiteralMethodDef {
  name: string;
  kind: MethodKind;
  params: ParamDef[];
  computed?: boolean;
  optional: boolean;
  returnType?: TsTypeDef;
  typeParams: TsTypeParamDef[];
  location?: Location;
}

export interface LiteralPropertyDef {
  name: string;
  params: ParamDef[];
  readonly?: boolean;
  computed: boolean;
  optional: boolean;
  tsType?: TsTypeDef;
  typeParams: TsTypeParamDef[];
  location?: Location;
}

export interface Location {
  filename: string;
  line: number;
  col: number;
  byteIndex?: number;
}

export type MethodKind = "method" | "getter" | "setter";

export interface NamespaceDef {
  elements: DocNode[];
}

export type ObjectPatPropDef =
  | ObjectPatPropAssignDef
  | ObjectPatPropKeyValueDef
  | ObjectPatPropRestDef;

export interface ObjectPatPropAssignDef {
  kind: "assign";
  key: string;
  value?: string;
}

export interface ObjectPatPropKeyValueDef {
  kind: "keyValue";
  key: string;
  value: ParamDef;
}

export interface ObjectPatPropRestDef {
  kind: "rest";
  arg: ParamDef;
}

export type ParamDef =
  | ParamArrayDef
  | ParamAssignDef
  | ParamIdentifierDef
  | ParamObjectDef
  | ParamRestDef;

export interface ParamArrayDef {
  kind: "array";
  elements: (ParamDef | undefined)[];
  optional: boolean;
  decorators?: DecoratorDef[];
  tsType?: TsTypeDef;
}

export interface ParamAssignDef {
  kind: "assign";
  left: ParamDef;
  right: string;
  decorators?: DecoratorDef[];
  tsType?: TsTypeDef;
}

export interface ParamIdentifierDef {
  kind: "identifier";
  name: string;
  optional: boolean;
  decorators?: DecoratorDef[];
  tsType?: TsTypeDef;
}

export interface ParamObjectDef {
  kind: "object";
  props: ObjectPatPropDef[];
  optional: boolean;
  decorators?: DecoratorDef[];
  tsType?: TsTypeDef;
}

export interface ParamRestDef {
  kind: "rest";
  arg: ParamDef;
  decorators?: DecoratorDef[];
  tsType?: TsTypeDef;
}

export type TruePlusMinus = true | "+" | "-";

export interface TsConditionalDef {
  checkType: TsTypeDef;
  extendsType: TsTypeDef;
  trueType: TsTypeDef;
  falseType: TsTypeDef;
}

export interface TsFnOrConstructorDef {
  constructor: boolean;
  tsType: TsTypeDef;
  params: ParamDef[];
  typeParams: TsTypeParamDef[];
}

export interface TsImportTypeDef {
  specifier: string;
  qualifier?: string;
  typeParams?: TsTypeDef[];
}

export interface TsIndexedAccessDef {
  readonly: boolean;
  objType: TsTypeDef;
  indexType: TsTypeDef;
}

export interface TsInferDef {
  typeParam: TsTypeParamDef;
}

export interface TsMappedTypeDef {
  readonly?: TruePlusMinus;
  typeParam: TsTypeParamDef;
  nameType?: TsTypeDef;
  optional?: TruePlusMinus;
  tsType?: TsTypeDef;
}

export interface TsTypeLiteralDef {
  methods: LiteralMethodDef[];
  properties: LiteralPropertyDef[];
  callSignatures: LiteralCallSignatureDef[];
  indexSignatures: LiteralIndexSignatureDef[];
  constructors?: unknown[];
}

export interface TsTypeOperatorDef {
  operator: string;
  tsType: TsTypeDef;
}

export interface TsTypeParamDef {
  name: string;
  constraint?: TsTypeDef;
  default?: TsTypeDef;
}

export interface TsTypePredicateDef {
  asserts: boolean;
  param: { type: "this" | "identifier"; name?: string };
  type?: TsTypeDef;
}

export type TsTypeDef =
  | TsTypeKeywordDef
  | TsTypeDefLiteral
  | TsTypeTypeRefDef
  | TsTypeUnionDef
  | TsTypeIntersectionDef
  | TsTypeArrayDef
  | TsTypeTupleDef
  | TsTypeTypeOperatorDef
  | TsTypeParenthesizedDef
  | TsTypeRestDef
  | TsTypeOptionalDef
  | TsTypeQueryDef
  | TsTypeThisDef
  | TsTypeFnOrConstructorDef
  | TsTypeConditionalDef
  | TsTypeImportTypeDef
  | TsTypeInferDef
  | TsTypeIndexedAccessDef
  | TsTypeMappedDef
  | TsTypeTypeLiteralDef
  | TsTypeTypePredicateDef;

interface TsTypeDefBase {
  repr: string;
  kind: TsTypeDefKind;
}

export interface TsTypeKeywordDef extends TsTypeDefBase {
  kind: "keyword";
  value: string;
}

export interface TsTypeDefLiteral extends TsTypeDefBase {
  kind: "literal";
  value: LiteralDef;
}

export interface TsTypeTypeRefDef extends TsTypeDefBase {
  kind: "typeRef";
  value: TsTypeRefDef;
}

export interface TsTypeUnionDef extends TsTypeDefBase {
  kind: "union";
  value: TsTypeDef[];
}

export interface TsTypeIntersectionDef extends TsTypeDefBase {
  kind: "intersection";
  value: TsTypeDef[];
}

export interface TsTypeArrayDef extends TsTypeDefBase {
  kind: "array";
  value: TsTypeDef;
}

export interface TsTypeTupleDef extends TsTypeDefBase {
  kind: "tuple";
  value: TsTypeDef[];
}

export interface TsTypeTypeOperatorDef extends TsTypeDefBase {
  kind: "typeOperator";
  value: TsTypeOperatorDef;
}

export interface TsTypeParenthesizedDef extends TsTypeDefBase {
  kind: "parenthesized";
  value: TsTypeDef;
}

export interface TsTypeRestDef extends TsTypeDefBase {
  kind: "rest";
  value: TsTypeDef;
}

export interface TsTypeOptionalDef extends TsTypeDefBase {
  kind: "optional";
  value: TsTypeDef;
}

export interface TsTypeQueryDef extends TsTypeDefBase {
  kind: "typeQuery";
  value: string;
}

export interface TsTypeThisDef extends TsTypeDefBase {
  kind: "this";
}

export interface TsTypeFnOrConstructorDef extends TsTypeDefBase {
  kind: "fnOrConstructor";
  value: TsFnOrConstructorDef;
}

export interface TsTypeConditionalDef extends TsTypeDefBase {
  kind: "conditional";
  value: TsConditionalDef;
}

export interface TsTypeInferDef extends TsTypeDefBase {
  kind: "infer";
  value: TsInferDef;
}

export interface TsTypeMappedDef extends TsTypeDefBase {
  kind: "mapped";
  value: TsMappedTypeDef;
}

export interface TsTypeImportTypeDef extends TsTypeDefBase {
  kind: "importType";
  value: TsImportTypeDef;
}

export interface TsTypeIndexedAccessDef extends TsTypeDefBase {
  kind: "indexedAccess";
  value: TsIndexedAccessDef;
}

export interface TsTypeTypeLiteralDef extends TsTypeDefBase {
  kind: "typeLiteral";
  value: TsTypeLiteralDef;
}

export interface TsTypeTypePredicateDef extends TsTypeDefBase {
  kind: "typePredicate";
  value: TsTypePredicateDef;
}

export type TsTypeDefKind =
  | "keyword"
  | "literal"
  | "typeRef"
  | "union"
  | "intersection"
  | "array"
  | "tuple"
  | "typeOperator"
  | "parenthesized"
  | "rest"
  | "optional"
  | "typeQuery"
  | "this"
  | "fnOrConstructor"
  | "conditional"
  | "importType"
  | "infer"
  | "indexedAccess"
  | "mapped"
  | "typeLiteral"
  | "typePredicate";

export interface TsTypeRefDef {
  typeParams?: TsTypeDef[];
  typeName: string;
}

export interface TypeAliasDef {
  tsType: TsTypeDef;
  typeParams: TsTypeParamDef[];
}

export type VariableDeclKind = "var" | "let" | "const";

export interface VariableDef {
  tsType?: TsTypeDef;
  kind: VariableDeclKind;
}

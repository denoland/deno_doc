// Copyright 2020-2021 the Deno authors. All rights reserved. MIT license.

export type DocNode =
  | DocNodeFunction
  | DocNodeVariable
  | DocNodeEnum
  | DocNodeClass
  | DocNodeTypeAlias
  | DocNodeNamespace
  | DocNodeInterface
  | DocNodeImport;

interface DocNodeBase {
  kind: DocNodeKind;
  name: string;
  location: Location;
  jsDoc?: string;
}

export type DocNodeKind =
  | "function"
  | "variable"
  | "enum"
  | "class"
  | "typeAlias"
  | "namespace"
  | "interface"
  | "import";

export interface DocNodeFunction extends DocNodeBase {
  kind: "function";
  functionDef: FunctionDef;
}

export interface DocNodeVariable extends DocNodeBase {
  kind: "variable";
  variableDef: VariableDef;
}

export interface DocNodeEnum extends DocNodeBase {
  kind: "enum";
  enumDef: EnumDef;
}

export interface DocNodeClass extends DocNodeBase {
  kind: "class";
  classDef: ClassDef;
}

export interface DocNodeTypeAlias extends DocNodeBase {
  kind: "typeAlias";
  typeAliasDef: TypeAliasDef;
}

export interface DocNodeNamespace extends DocNodeBase {
  kind: "namespace";
  namespaceDef: NamespaceDef;
}

export interface DocNodeInterface extends DocNodeBase {
  kind: "interface";
  interfaceDef: InterfaceDef;
}

export interface DocNodeImport extends DocNodeBase {
  kind: "import";
  importDef: ImportDef;
}

export type Accessibility = "public" | "protected" | "private";

export interface ClassDef {
  isAbstract: boolean;
  constructors: ClassConstructorDef[];
  properties: ClassPropertyDef[];
  indexSignatures: ClassIndexSignatureDef[];
  methods: ClassMethodDef[];
  extends?: string;
  implements: TsTypeDef[];
  typeParams: TsTypeParamDef[];
  superTypeParams: TsTypeDef[];
}

export interface ClassConstructorDef {
  jsDoc?: string;
  accessibility?: Accessibility;
  name: string;
  params: ParamDef[];
  location: Location;
}

export interface ClassIndexSignatureDef {
  readonly: boolean;
  params: ParamDef[];
  tsType?: TsTypeDef;
}

export interface ClassMethodDef {
  jsDoc?: string;
  accessibility?: Accessibility;
  optional: boolean;
  isAbstract: boolean;
  isStatic: boolean;
  name: string;
  kind: MethodKind;
  functionDef: FunctionDef;
  location: Location;
}

export interface ClassPropertyDef {
  jsDoc?: string;
  tsType?: TsTypeDef;
  readonly: boolean;
  accessibility?: Accessibility;
  optional: boolean;
  isAbstract: boolean;
  isStatic: boolean;
  name: string;
  location: Location;
}

export interface EnumDef {
  members: EnumMemberDef[];
}

export interface EnumMemberDef {
  name: string;
  jsDoc?: string;
}

export interface FunctionDef {
  params: ParamDef[];
  returnType?: TsTypeDef;
  isAsync: boolean;
  isGenerator: boolean;
  typeParams: TsTypeParamDef[];
}

export interface ImportDef {
  src: string;
  imported?: string;
}

export interface InterfaceDef {
  extends: TsTypeDef[];
  methods: InterfaceMethodDef[];
  properties: InterfacePropertyDef[];
  callSignatures: InterfaceCallSignatureDef[];
  indexSignatures: InterfaceIndexSignatureDef[];
  typeParams: TsTypeParamDef[];
}

export interface InterfaceCallSignatureDef {
  location: Location;
  jsDoc?: string;
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
  jsDoc?: string;
  optional: boolean;
  params: ParamDef[];
  returnType?: TsTypeDef;
  typeParams: TsTypeParamDef[];
}

export interface InterfacePropertyDef {
  name: string;
  location: Location;
  jsDoc?: string;
  params: ParamDef[];
  computed: boolean;
  optional: boolean;
  tsType?: TsTypeDef;
  typeParams: TsTypeParamDef[];
}

export interface LiteralCallSignatureDef {
  params: ParamDef[];
  tsType?: TsTypeDef;
  typeParams: TsTypeParamDef[];
}

export type LiteralDefKind = "number" | "string" | "boolean" | "bigInt";

export type LiteralDef =
  | LiteralDefNumber
  | LiteralDefBigInt
  | LiteralDefString
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
  number: bigint;
}

export interface LiteralDefString extends LiteralDefBase {
  kind: "string";
  string: string;
}

export interface LiteralDefBoolean extends LiteralDefBase {
  kind: "boolean";
  boolean: boolean;
}

export interface LiteralIndexSignatureDef {
  readonly: boolean;
  params: ParamDef[];
  tsType?: TsTypeDef;
}

export interface LiteralMethodDef {
  name: string;
  params: ParamDef[];
  returnType?: TsTypeDef;
  typeParams: TsTypeParamDef[];
}

export interface LiteralPropertyDef {
  name: string;
  params: ParamDef[];
  computed: boolean;
  optional: boolean;
  tsType?: TsTypeDef;
  typeParams: TsTypeParamDef[];
}

export interface Location {
  filename: string;
  line: number;
  col: number;
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
  tsType?: TsTypeDef;
}

export interface ParamAssignDef {
  kind: "assign";
  left: ParamDef;
  right: string;
  tsType?: TsTypeDef;
}

export interface ParamIdentifierDef {
  kind: "identifier";
  name: string;
  optional: boolean;
  tsType?: TsTypeDef;
}

export interface ParamObjectDef {
  kind: "object";
  props: ObjectPatPropDef[];
  optional: boolean;
  tsType?: TsTypeDef;
}

export interface ParamRestDef {
  kind: "rest";
  arg: ParamDef;
  tsType?: TsTypeDef;
}

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

export interface TsIndexedAccessDef {
  readonly: boolean;
  objType: TsTypeDef;
  indexType: TsTypeDef;
}

export interface TsTypeLiteralDef {
  methods: LiteralMethodDef[];
  properties: LiteralPropertyDef[];
  callSignatures: LiteralCallSignatureDef[];
  indexSignatures: LiteralIndexSignatureDef[];
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
  | TsTypeIndexedAccessDef
  | TsTypeTypeLiteralDef
  | TsTypeTypePredicateDef;

interface TsTypeDefBase {
  repr: string;
  kind: TsTypeDefKind;
}

export interface TsTypeKeywordDef extends TsTypeDefBase {
  kind: "keyword";
  keyword: string;
}

export interface TsTypeDefLiteral extends TsTypeDefBase {
  kind: "literal";
  literal: LiteralDef;
}

export interface TsTypeTypeRefDef extends TsTypeDefBase {
  kind: "typeRef";
  typeRef: TsTypeRefDef;
}

export interface TsTypeUnionDef extends TsTypeDefBase {
  kind: "union";
  union: TsTypeDef[];
}

export interface TsTypeIntersectionDef extends TsTypeDefBase {
  kind: "intersection";
  intersection: TsTypeDef[];
}

export interface TsTypeArrayDef extends TsTypeDefBase {
  kind: "array";
  array: TsTypeDef;
}

export interface TsTypeTupleDef extends TsTypeDefBase {
  kind: "tuple";
  tuple: TsTypeDef[];
}

export interface TsTypeTypeOperatorDef extends TsTypeDefBase {
  kind: "typeOperator";
  typeOperator: TsTypeOperatorDef;
}

export interface TsTypeParenthesizedDef extends TsTypeDefBase {
  kind: "parenthesized";
  parenthesized: TsTypeDef;
}

export interface TsTypeRestDef extends TsTypeDefBase {
  kind: "rest";
  rest: TsTypeDef;
}

export interface TsTypeOptionalDef extends TsTypeDefBase {
  kind: "optional";
  optional: TsTypeDef;
}

export interface TsTypeQueryDef extends TsTypeDefBase {
  kind: "typeQuery";
  typeQuery: string;
}

export interface TsTypeThisDef extends TsTypeDefBase {
  kind: "this";
  this: boolean;
}

export interface TsTypeFnOrConstructorDef extends TsTypeDefBase {
  kind: "fnOrConstructor";
  fnOrConstructor: TsFnOrConstructorDef;
}

export interface TsTypeConditionalDef extends TsTypeDefBase {
  kind: "conditional";
  conditionalType: TsConditionalDef;
}

export interface TsTypeIndexedAccessDef extends TsTypeDefBase {
  kind: "indexedAccess";
  indexedAccess: TsIndexedAccessDef;
}

export interface TsTypeTypeLiteralDef extends TsTypeDefBase {
  kind: "typeLiteral";
  typeLiteral: TsTypeLiteralDef;
}

export interface TsTypeTypePredicateDef extends TsTypeDefBase {
  kind: "typePredicate";
  typePredicate: TsTypePredicateDef;
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
  | "indexedAccess"
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

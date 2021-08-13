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
  kind: DocNodeType;
  name: string;
  location: Location;
  jsDoc?: string;
}

type DocNodeType =
  | "function"
  | "variable"
  | "enum"
  | "class"
  | "typeAlias"
  | "namespace"
  | "interface"
  | "import";

interface DocNodeFunction extends DocNodeBase {
  kind: "function";
  functionDef: FunctionDef;
}

interface DocNodeVariable extends DocNodeBase {
  kind: "variable";
  variableDef: VariableDef;
}

interface DocNodeEnum extends DocNodeBase {
  kind: "enum";
  enumDef: EnumDef;
}

interface DocNodeClass extends DocNodeBase {
  kind: "class";
  classDef: ClassDef;
}

interface DocNodeTypeAlias extends DocNodeBase {
  kind: "typeAlias";
  typeAliasDef: TypeAliasDef;
}

interface DocNodeNamespace extends DocNodeBase {
  kind: "namespace";
  namespaceDef: NamespaceDef;
}

interface DocNodeInterface extends DocNodeBase {
  kind: "interface";
  interfaceDef: InterfaceDef;
}

interface DocNodeImport extends DocNodeBase {
  kind: "import";
  importDef: ImportDef;
}

type Accessibility = "public" | "protected" | "private";

interface ClassDef {
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

interface ClassConstructorDef {
  jsDoc?: string;
  accessibility?: Accessibility;
  name: string;
  params: ParamDef[];
  location: Location;
}

interface ClassIndexSignatureDef {
  readonly: boolean;
  params: ParamDef[];
  tsType?: TsTypeDef;
}

interface ClassMethodDef {
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

interface ClassPropertyDef {
  jsDoc?: string;
  tsType?: TsTypeDef;
  readonly: boolean;
  accessibility?: Accessibility;
  optional: boolean;
  isAbstract: boolean;
  name: string;
  location: Location;
}

interface EnumDef {
  members: EnumMemberDef[];
}

interface EnumMemberDef {
  name: string;
  jsDoc?: string;
}

interface FunctionDef {
  params: ParamDef[];
  returnType?: TsTypeDef;
  isAsync: boolean;
  isGenerator: boolean;
  typeParams: TsTypeParamDef[];
}

interface ImportDef {
  src: string;
  imported?: string;
}

interface InterfaceDef {
  extends: TsTypeDef[];
  methods: InterfaceMethodDef[];
  properties: InterfacePropertyDef[];
  callSignatures: InterfaceCallSignatureDef[];
  indexSignatures: InterfaceIndexSignatureDef[];
  typeParams: TsTypeParamDef[];
}

interface InterfaceCallSignatureDef {
  location: Location;
  jsDoc?: string;
  params: ParamDef[];
  tsType?: TsTypeDef;
  typeParams: TsTypeParamDef[];
}

interface InterfaceIndexSignatureDef {
  readonly: boolean;
  params: ParamDef[];
  tsType?: TsTypeDef;
}

interface InterfaceMethodDef {
  name: string;
  location: Location;
  jsDoc?: string;
  optional: boolean;
  params: ParamDef[];
  returnType?: TsTypeDef;
  typeParams: TsTypeParamDef[];
}

interface InterfacePropertyDef {
  name: string;
  location: Location;
  jsDoc?: string;
  params: ParamDef[];
  computed: boolean;
  optional: boolean;
  tsType?: TsTypeDef;
  typeParams: TsTypeParamDef[];
}

interface LiteralCallSignatureDef {
  params: ParamDef[];
  tsType?: TsTypeDef;
  typeParams: TsTypeParamDef[];
}

type LiteralDefKind = "number" | "string" | "boolean" | "bigInt";

type LiteralDef =
  | LiteralDefNumber
  | LiteralDefBigInt
  | LiteralDefString
  | LiteralDefBoolean;

interface LiteralDefBase {
  kind: LiteralDefKind;
}

interface LiteralDefNumber extends LiteralDefBase {
  kind: "number";
  number: number;
}

interface LiteralDefBigInt extends LiteralDefBase {
  kind: "bigInt";
  number: bigint;
}

interface LiteralDefString extends LiteralDefBase {
  kind: "string";
  string: string;
}

interface LiteralDefBoolean extends LiteralDefBase {
  kind: "boolean";
  boolean: boolean;
}

interface LiteralIndexSignatureDef {
  readonly: boolean;
  params: ParamDef[];
  tsType?: TsTypeDef;
}

interface LiteralMethodDef {
  name: string;
  params: ParamDef[];
  returnType?: TsTypeDef;
  typeParams: TsTypeParamDef[];
}

interface LiteralPropertyDef {
  name: string;
  params: ParamDef[];
  computed: boolean;
  optional: boolean;
  tsType?: TsTypeDef;
  typeParams: TsTypeParamDef[];
}

interface Location {
  filename: string;
  line: number;
  col: number;
}

type MethodKind = "method" | "getter" | "setter";

interface NamespaceDef {
  elements: DocNode[];
}

type ObjectPatPropDef = ObjectPatPropAssignDef;

interface ObjectPatPropAssignDef {
  kind: "assign";
  key: string;
  value?: string;
}

interface ObjectPatPropKeyValueDef {
  kind: "keyValue";
  key: string;
  value: ParamDef;
}

interface ObjectPatPropRestDef {
  kind: "rest";
  arg: ParamDef;
}

type ParamDef =
  | ParamArrayDef
  | ParamAssignDef
  | ParamIdentifierDef
  | ParamObjectDef
  | ParamRestDef;

interface ParamArrayDef {
  kind: "array";
  elements: (ParamDef | undefined)[];
  optional: boolean;
  tsType?: TsTypeDef;
}

interface ParamAssignDef {
  kind: "assign";
  left: ParamDef;
  right: string;
  tsType?: TsTypeDef;
}

interface ParamIdentifierDef {
  kind: "identifier";
  name: string;
  optional: boolean;
  tsType?: TsTypeDef;
}

interface ParamObjectDef {
  kind: "object";
  props: ObjectPatPropDef[];
  optional: boolean;
  tsType?: TsTypeDef;
}

interface ParamRestDef {
  kind: "rest";
  arg: ParamDef;
  tsType?: TsTypeDef;
}

interface TsConditionalDef {
  checkType: TsTypeDef;
  extendsType: TsTypeDef;
  trueType: TsTypeDef;
  falseType: TsTypeDef;
}

interface TsFnOrConstructorDef {
  constructor: boolean;
  tsType: TsTypeDef;
  params: ParamDef[];
  typeParams: TsTypeParamDef[];
}

interface TsIndexedAccessDef {
  readonly: boolean;
  objType: TsTypeDef;
  indexType: TsTypeDef;
}

interface TsTypeLiteralDef {
  methods: LiteralMethodDef[];
  properties: LiteralPropertyDef[];
  callSignatures: LiteralCallSignatureDef[];
  indexSignatures: LiteralIndexSignatureDef[];
}

interface TsTypeOperatorDef {
  operator: string;
  tsType: TsTypeDef;
}

interface TsTypeParamDef {
  name: string;
  constraint?: TsTypeDef;
  default?: TsTypeDef;
}

interface TsTypePredicateDef {
  asserts: boolean;
  param: { type: "this" | "identifier"; name?: string };
  type?: TsTypeDef;
}

type TsTypeDef =
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

interface TsTypeKeywordDef extends TsTypeDefBase {
  kind: "keyword";
  keyword: string;
}

interface TsTypeDefLiteral extends TsTypeDefBase {
  kind: "literal";
  literal: LiteralDef;
}

interface TsTypeTypeRefDef extends TsTypeDefBase {
  kind: "typeRef";
  typeRef: TsTypeRefDef;
}

interface TsTypeUnionDef extends TsTypeDefBase {
  kind: "union";
  union: TsTypeDef[];
}

interface TsTypeIntersectionDef extends TsTypeDefBase {
  kind: "intersection";
  intersection: TsTypeDef[];
}

interface TsTypeArrayDef extends TsTypeDefBase {
  kind: "array";
  array: TsTypeDef;
}

interface TsTypeTupleDef extends TsTypeDefBase {
  kind: "tuple";
  tuple: TsTypeDef[];
}

interface TsTypeTypeOperatorDef extends TsTypeDefBase {
  kind: "typeOperator";
  typeOperator: TsTypeOperatorDef;
}

interface TsTypeParenthesizedDef extends TsTypeDefBase {
  kind: "parenthesized";
  parenthesized: TsTypeDef;
}

interface TsTypeRestDef extends TsTypeDefBase {
  kind: "rest";
  rest: TsTypeDef;
}

interface TsTypeOptionalDef extends TsTypeDefBase {
  kind: "optional";
  optional: TsTypeDef;
}

interface TsTypeQueryDef extends TsTypeDefBase {
  kind: "typeQuery";
  typeQuery: string;
}

interface TsTypeThisDef extends TsTypeDefBase {
  kind: "this";
  this: boolean;
}

interface TsTypeFnOrConstructorDef extends TsTypeDefBase {
  kind: "fnOrConstructor";
  fnOrConstructor: TsFnOrConstructorDef;
}

interface TsTypeConditionalDef extends TsTypeDefBase {
  kind: "conditional";
  conditionalType: TsConditionalDef;
}

interface TsTypeIndexedAccessDef extends TsTypeDefBase {
  kind: "indexedAccess";
  indexedAccess: TsIndexedAccessDef;
}

interface TsTypeTypeLiteralDef extends TsTypeDefBase {
  kind: "typeLiteral";
  typeLiteral: TsTypeLiteralDef;
}

interface TsTypeTypePredicateDef extends TsTypeDefBase {
  kind: "typePredicate";
  typePredicate: TsTypePredicateDef;
}

type TsTypeDefKind =
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

interface TsTypeRefDef {
  typeParams?: TsTypeDef[];
  typeName: string;
}

interface TypeAliasDef {
  tsType: TsTypeDef;
  typeParams: TsTypeParamDef[];
}

type VariableDeclKind = "var" | "let" | "const";

interface VariableDef {
  tsType?: TsTypeDef;
  kind: VariableDeclKind;
}

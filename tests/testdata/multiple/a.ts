import { x } from "./c.ts";
import { externalFunction } from "./_d.ts";

/**
 * Some docs
 * with a line break
 *
 * @deprecated
 * @module
 */

/**
 * some Foo docs {@linkcode Bar}
 *
 * @example test
 *
 * hello
 *
 * bar
 *
 * ```ts
 * foo
 * ```
 *
 * baz
 *
 * @example test 2
 *
 * hello
 *
 * bar
 *
 * ```ts
 * foo
 * ```
 *
 * baz
 */
export class Foo<T> {
  constructor(value: T) {}

  static bar: "string";
  protected protectedProperty = false;
  readonly readonlyProperty = false;
  foo?: A = A;
  '"><img src=x onerror=alert(1)>' = 0;
  [Symbol.iterator]() {}

  protected get getter() {
    return "";
  }

  set setter(s: string) {
  }

  get getterAndSetter() {
    return "";
  }
  set getterAndSetter(s) {
  }

  static set staticSetter(s: number) {}

  /**
   * @throws
   */
  test() {}

  static staticMethod() {}

  private methodWithOverloads(s: number): void;
  private methodWithOverloads(s: string): void;
  private methodWithOverloads(s: number | string): void {}
}

/**
 * > Some quote in bar docs
 * > This quote part is ignored
 * > when getting the title of this doc
 *
 * Bar docs
 */
export class Bar extends Foo<string> {
}

/**
 * ```ts
 * // This code block is ignored when getting the title of this doc
 * const foobar = new Foobar();
 * ```
 *
 * Foobar docs
 *
 * # heading
 *
 * content
 *
 * ## sub heading
 *
 * @see https://example.com
 * @priority 10
 */
export default class Foobar {
}

export abstract class AbstractClass {
  abstract foo: string;
  abstract method?(s: number | string): s is string;
  abstract get getter(): string;
}

/**
 * @priority 5
 */
export interface Hello<T extends string, E extends T, R = number> {
  (a: string): string;
  /**
   * @default {"foo"}
   */
  world: string;
  /**
   * Some docs
   */
  readonly test?: "test";
  ["ab"]: string;
  new <T extends string, E extends T, R = number>(): Hello<T, E, R>;
  optionalMethod?(): [string?];
  ["computedMethod"]?(a: T extends () => infer R ? R : any): void;

  x: {
    [foo: string]: number;
  };
}

export interface EmptyInterface {}

export interface InterfaceWithIndexSignature {
  [foo: string]: `foo${string}` | (typeof Foobar) & number & this;
}

export type Baz<T> = {
  foo: Record<string, T extends string ? 0 : 1>;
  bar(): InterfaceWithIndexSignature["test"];
};

export type TypeAlias = string;

class A {}

/** @internal */
export class B {}

/**
 * content
 *
 * @deprecated content
 */
export function qaz(a: string);
export function qaz(a: number);
export function qaz(a: string | number) {}

export const c = (): string => "hello";

/**
 * @param foo {number} some parameter
 * @returns a new string
 * @throws {Foo} bar
 * @example
 * test
 * ```ts
 * d();
 * ```
 * @example
 * ```ts
 * d();
 * ```
 */
export function d<T = string>(
  foo: number = 1,
  bar: string = "bar",
  baz: { hello?: string } = {},
  qaz: T,
  ...strings: string[]
): string {
  return foo + bar;
}

/**
 * @throws bar
 * @see anotherVariable
 */
export function functionWithOptionalParameters(
  foo: number = 1,
  bar?: number,
  baz?: [number],
  [qaz]?: string[],
  { qux }?: { qux: number },
) {
}

export enum Enum {
  Foo = "foo",
  Bar = "bar",
}

/**
 * description to be overwritten
 * @summary the summary
 * @description the description
 */
export enum Enum2 {
  Foo,
  Bar,
}

export let someVariable;
export let anotherVariable: { foo: string; bar(): number };

export class Testing {}

Testing.x = x;
Testing.externalFunction = externalFunction;

export declare namespace Testing {
  export { externalFunction, x };

  export function t(): string;
}

export { x };

export interface CompoundType {
  readonly bufferedAmount: number;
}

export const CompoundType = {};

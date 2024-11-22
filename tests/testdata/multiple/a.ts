/**
 * Some docs
 * with a line break
 *
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
export class Foo {
  static bar: "string";
  foo: A = A;
  '"><img src=x onerror=alert(1)>' = 0;
  test() {}
}

/**
 * > Some quote in bar docs
 * > This quote part is ignored
 * > when getting the title of this doc
 *
 * Bar docs
 */
export class Bar extends Foo {
}

/**
 * ```ts
 * // This code block is ignored when getting the title of this doc
 * const foobar = new Foobar();
 * ```
 *
 * Foobar docs
 *
 * @see https://example.com
 */
export default class Foobar {
}

export interface Hello {
  world: "string";
}

export type Baz = {
  foo: string;
};

class A {}

/** @internal */
export class B {}

/**
 * @deprecated
 */
export function qaz(a: string);
export function qaz(a: number);
export function qaz(a: string | number) {}

export const c = (): string => "hello";

/**
 * @throws {Foo} bar
 */
export function d(
  foo: number = 1,
  bar: string = "bar",
  baz: { hello?: string } = {},
): string {
  return foo + bar;
}

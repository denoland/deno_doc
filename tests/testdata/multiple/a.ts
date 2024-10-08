/**
 * Some docs
 *
 * > [!NOTE]
 * > Useful information that users should know, even when skimming content.
 *
 * > [!TIP]
 * > Helpful advice for doing things better or more easily.
 *
 * > [!IMPORTANT]
 * > Key information users need to know to achieve their goal.
 *
 * > [!WARNING]
 * > Urgent info that needs immediate user attention to avoid problems.
 *
 * > [!CAUTION]
 * > Advises about risks or negative outcomes of certain actions.
 *
 * @module
 */

/**
 * some Foo docs {@linkcode Bar}
 */
export class Foo {
  static bar: "string";
  foo: A = A;
  '"><img src=x onerror=alert(1)>' = 0;
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

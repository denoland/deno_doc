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
 * some Foo docs
 */
export class Foo {
  static bar: "string";
  foo: A = A;
  '"><img src=x onerror=alert(1)>' = 0;
}

export class Bar extends Foo {
}

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

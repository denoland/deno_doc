/**
 * ```ts
 * using time = new FakeTime();
 * ```
 */
export class Foo {}

export class Bar extends Foo {}

// deno-lint-ignore no-empty-interface
interface NotExported {}

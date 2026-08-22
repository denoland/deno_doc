/** Constructor interface for examples. */
export interface ExampleConstructor {
  /**
   * Example constructor
   * @param a The first number
   * @returns The example object
   * @example
   * ```ts
   * const example = new Example(1);
   * ```
   */
  new (a: number): Example;
}

/**
 * Object type for the example
 */
export interface Example {
  a: number;
}

export const Example: ExampleConstructor = class {
  public a: number;
  constructor(a: number) {
    this.a = a;
  }
};

/** Callable interface for examples. */
export interface ExampleCallable {
  /**
   * Calls the example.
   * @example
   * ```ts
   * exampleCallable(2);
   * ```
   */
  (a: number): number;
}

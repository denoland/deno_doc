export function topLevelFn(): void {}

export interface TopLevelInterface {
  a: string;
}

// deno-lint-ignore no-namespace
export namespace outer {
  export function outerFnOne(): void {}
  export function outerFnTwo(): void {}
  export function outerFnThree(): void {}

  // deno-lint-ignore no-namespace
  export namespace inner {
    export function innerFnOne(): void {}
    export function innerFnTwo(): void {}
    export function innerFnThree(): void {}
  }
}

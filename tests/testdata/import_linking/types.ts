export interface Expression {
  kind: string;
}

export interface Statement {
  kind: string;
}

type Internal = Expression;

export class Scope {
  expr!: Internal;
}

// deno-lint-ignore no-namespace
export namespace Scope {
  export type Alias = Internal;
}

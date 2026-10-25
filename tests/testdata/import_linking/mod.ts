import type * as t from "./types.ts";
import type { Statement as Stmt } from "./types.ts";

/**
 * See {@link t.Expression} and {@link Stmt}.
 */
export function expression(input: t.Expression): t.Expression {
  return input;
}

export function statement(input: Stmt): Stmt {
  return input;
}

// deno-lint-ignore no-explicit-any
export function missing(_input: t.DoesNotExist): any {}

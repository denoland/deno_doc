/**
 * @module bar
 */

export function x(_n = 0) {}

/** The default export item.
 *
 * This item reproduces the issue reported in {@link https://github.com/jsr-io/jsr/issues/459}
 */
const default_: number = 0;
export { default_ as default };

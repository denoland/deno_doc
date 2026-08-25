export type Matrix = [
  [number, number, number],
  [number, number, number],
  [number, number, number],
];
export type Vector = [number, number, number];

/**
 * Transforms a vector by a matrix.
 *
 * @param r - A transformation matrix.
 * @param v - A vector.
 *
 * @returns The transformed vector.
 */
export function transform(
  [[r11, r12, r13], [r21, r22, r23], [r31, r32, r33]]: Matrix,
  [x, y, z]: Vector,
): Vector {
  return [
    r11 * x + r12 * y + r13 * z,
    r21 * x + r22 * y + r23 * z,
    r31 * x + r32 * y + r33 * z,
  ];
}

/**
 * A named parameter must keep its own documentation.
 *
 * @param scale The scale factor.
 */
export function scaleOnly(scale: number, [x, y]: Vector): Vector {
  return [x * scale, y * scale, 0];
}

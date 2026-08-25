class OptionClass<T> {
  /** Returns the contained value. */
  unwrap(): T {
    throw new Error("empty");
  }
}

/** An Option-like structure. */
export const Option = OptionClass;

/** greets */
function greet(name: string): string {
  return name;
}

/** Says hello. */
export const sayHello = greet;

/**
 * A domestic animal.
 *
 * This class represents a generic domestic animal with basic properties
 * and behaviors. Animals can vocalize, be fed, and have their information
 * displayed. Now supports color tracking and descriptions.
 *
 * ## Usage
 *
 * ```ts
 * const dog = new Animal("Rex", 5, "brown");
 * dog.vocalize();
 * dog.feed("kibble", 2);
 * dog.describe();
 * ```
 *
 * ## Lifecycle
 *
 * 1. Create an animal with `new Animal(name, age, color?)`
 * 2. Set up the species and weight
 * 3. Use `vocalize()` and `feed()` methods
 * 4. Get descriptions with `describe()`
 * 5. Check status with `displayName`
 */
export class Animal<T = string> {
  /** The animal's name. */
  name: string;
  /**
   * The animal's age in months.
   *
   * Must be a non-negative integer. This was changed from years to months
   * for better precision. Fractional ages are rounded down.
   */
  age: number;
  readonly species: T;
  protected weight: number;
  /** The animal's color. */
  color: string;

  constructor(name: string, age: number, color?: string) {}

  /** Make the animal speak. */
  vocalize(): string { return ""; }
  /**
   * Feed the animal with amount.
   *
   * Provides food to the animal in the specified quantity. The type
   * of food should match the animal's dietary requirements.
   *
   * ## Supported Foods
   *
   * - Kibble: Standard dry food
   * - Wet food: Canned varieties
   * - Raw food: Fresh meat and vegetables
   * - Treats: Use sparingly
   *
   * ## Portion Guidelines
   *
   * Small animals: 1-2 portions per meal.
   * Large animals: 3-5 portions per meal.
   *
   * @param food The type of food to give.
   * @param amount Number of portions.
   */
  feed(food: string, amount: number): void {}
  private internalMethod(): void {}

  /** Describe the animal. */
  describe(verbose?: boolean): string { return ""; }

  static create(name: string, color?: string): Animal { return new Animal(name, 0); }
  static readonly MAX_AGE: number;

  get displayName(): string { return ""; }
  set displayName(value: string) {}
}

/**
 * A cat extending Animal.
 * @deprecated Use Dog instead.
 */
export class Cat extends Animal<"cat"> {
  purr(): void {}
}

/**
 * Base for renderers. No longer abstract.
 *
 * Provides the foundation for all rendering implementations.
 * Now includes concrete default implementations for all methods.
 *
 * ## Performance
 *
 * The base class handles buffering and frame timing. The default
 * render implementation uses a software rasterizer.
 *
 * ## Migration from AbstractRenderer
 *
 * If you were subclassing AbstractRenderer, you can now use this
 * class directly or override specific methods as needed.
 */
export class AbstractRenderer {
  constructor(width: number, height: string) {}
  render(input: string): string { return ""; }
  get canvas(): HTMLElement { return null as any; }
  format(input: string): string { return input; }
}

/** Serialization interface. */
export interface Serializable<T> {
  /**
   * Convert to JSON string with optional pretty printing.
   *
   * Serializes the object into a JSON string representation.
   * The output is always valid JSON that can be parsed back.
   *
   * When `pretty` is true, the output is formatted with 2-space
   * indentation for human readability.
   */
  toJSON(pretty?: boolean): string;
  /** Parse from string. */
  fromString(input: string): T;
  readonly type: string;
  optional?: boolean;
  /** The serialization version. */
  version: number;
}

/** Extended serializable. */
export interface AdvancedSerializable<T> extends Serializable<T>, Disposable {
  new(data: Uint8Array): AdvancedSerializable<T>;
  toBinary(): Uint8Array;
  compress(level: number, format?: string): Uint8Array;
  /** Decompress binary data. */
  decompress(data: Uint8Array): T;
}

/** An empty interface. */
export interface EmptyMarker {}

/** Interface with index signature. */
export interface StringMap {
  [prop: string]: string | number;
}

/** Color values. */
export enum Color {
  Red = "red",
  Green = "green",
  Blue = "blue",
  Yellow = "yellow",
  /** Custom color. */
  Custom = "custom",
}

/** Log levels. */
export enum LogLevel {
  Trace,
  Debug,
  Info,
  Warn,
  Error,
  Fatal,
}

/** Application config. */
export type Config<T = unknown> = {
  debug: boolean;
  verbose: boolean;
  logLevel: LogLevel;
  settings: Record<string, T>;
};

/** A unique identifier. */
export type ID = string | number;

/**
 * Create the application.
 * @param name The app name.
 * @param config Optional config.
 */
export function createApp(name: string, config?: Partial<Config>): App;
export function createApp(config: Config): App;
export function createApp(name: string, config: Config, plugins: Plugin[]): App;
export function createApp(nameOrConfig: string | Config, config?: Partial<Config>, plugins?: Plugin[]): App {
  return {} as App;
}

/**
 * Validate input data.
 * @deprecated Use validateAsync() instead.
 * @param data The data to validate.
 */
export function validate<T>(input: unknown, schema: Schema<T>): input is T {
  return true as any;
}

/**
 * Validate input data asynchronously.
 * @param data The data to validate.
 */
export function validateAsync<T>(data: unknown, schema: Schema<T>): Promise<T> {
  return null as any;
}

/** The current version. */
export const VERSION: "2.0.0" = "2.0.0";

/** Maximum retry count. */
export const MAX_RETRIES = 5;

export let mutableState: { count: number; label: string; active: boolean };

/**
 * A parser interface (was a class).
 *
 * Defines the contract for parsing input strings into structured data.
 * Implementations may operate synchronously or asynchronously.
 *
 * ## Migration from Parser class
 *
 * The `Parser` class has been replaced with this interface.
 * Use `createParser()` to get a default implementation.
 */
export interface Parser<T = string> {
  parse(input: string): T;
  tryParse(input: string): T | null;
}

/** Status codes. */
export enum Status {
  Ok = 200,
  NotFound = 404,
  Error = 500,
}

/** Identity transform. */
export function transform<T>(input: T): T {
  return input;
}

/** Readonly options. */
export interface Options {
  readonly timeout: number;
  readonly retries: number;
}

/** A compound type (interface + const). */
export interface CompoundType {
  readonly id: string;
  readonly name: string;
  readonly description: string;
  process(): void;
  validate(): boolean;
}

export const CompoundType: { new(): CompoundType } = null as any;

/**
 * Logger utility with severity.
 *
 * Logs messages to the console with required severity levels
 * and optional structured metadata.
 *
 * ## Log Format
 *
 * ```
 * [TIMESTAMP] [LEVEL] message {metadata}
 * ```
 *
 * ## Metadata
 *
 * Pass a record of key-value pairs as the third argument to
 * include structured data in the log output.
 */
export function log(msg: string, level?: LogLevel, metadata?: Record<string, unknown>): void {}

/**
 * A new router class.
 */
export class Router {
  /** Add a route. */
  addRoute(path: string, handler: Function): void {}
  /** Remove a route. */
  removeRoute(path: string): void {}
  /** Handle incoming request. */
  handle(request: Request): Response { return null as any; }
}

/**
 * Middleware function type.
 */
export type Middleware = (req: Request, next: () => Response) => Response;

/**
 * Create middleware pipeline.
 * @param middlewares The middleware chain.
 */
export function createPipeline(...middlewares: Middleware[]): Middleware {
  return null as any;
}

/**
 * A domestic animal.
 * Now with better type support.
 */
export class Animal<T = string> {
  /** The animal's name. */
  name: string;
  /** The animal's age in months. */
  age: number;
  readonly species: T;
  protected weight: number;
  /** The animal's color. */
  color: string;

  constructor(name: string, age: number, color?: string) {}

  /** Make the animal speak. */
  speak(): string { return ""; }
  /** Feed the animal with amount. */
  feed(food: string, amount: number): void {}
  private internalMethod(): void {}

  /** Describe the animal. */
  describe(): string { return ""; }

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
 */
export class AbstractRenderer {
  constructor(width: number, height: string) {}
  render(input: string): string { return ""; }
  get canvas(): HTMLElement { return null as any; }
  format(input: string): string { return input; }
}

/** Serialization interface. */
export interface Serializable<T> {
  /** Convert to JSON string with optional pretty printing. */
  toJSON(pretty?: boolean): string;
  /** Parse from string. */
  fromString(input: string): T;
  readonly kind: string;
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
  [key: string]: string | number;
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
export function validate<T>(data: unknown, schema: Schema<T>): data is T {
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

/** Logger utility with severity. */
export function log(message: string, level?: LogLevel, metadata?: Record<string, unknown>): void {}

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

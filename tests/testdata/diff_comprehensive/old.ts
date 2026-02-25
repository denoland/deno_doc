/**
 * A domestic animal.
 *
 * This class represents a generic domestic animal with basic properties
 * and behaviors. Animals can speak, be fed, and have their information
 * displayed.
 *
 * ## Usage
 *
 * ```ts
 * const dog = new Animal("Rex", 5);
 * dog.speak();
 * dog.feed("kibble");
 * ```
 *
 * ## Lifecycle
 *
 * 1. Create an animal with `new Animal(name, age)`
 * 2. Set up the species and weight
 * 3. Use `speak()` and `feed()` methods
 * 4. Check status with `displayName`
 */
export class Animal<T = string> {
  /** The animal's name. */
  name: string;
  /**
   * The animal's age in years.
   *
   * Must be a non-negative integer. Fractional ages are rounded down.
   */
  age: number;
  readonly species: T;
  protected weight: number;

  constructor(name: string, age: number) {}

  /** Make the animal speak. */
  speak(): string { return ""; }
  /**
   * Feed the animal.
   *
   * Provides food to the animal. The type of food should match the
   * animal's dietary requirements.
   *
   * ## Supported Foods
   *
   * - Kibble: Standard dry food
   * - Wet food: Canned varieties
   * - Treats: Use sparingly
   *
   * @param food The type of food to give.
   */
  feed(food: string): void {}
  /** @deprecated Use speak() instead. */
  makeSound(): string { return ""; }
  private internalMethod(): void {}

  static create(name: string): Animal { return new Animal(name, 0); }
  static readonly MAX_AGE: number;

  get displayName(): string { return ""; }
  set displayName(value: string) {}

  /** The animal's full ID. */
  get fullId(): string { return ""; }
}

/** A cat extending Animal. */
export class Cat extends Animal<"cat"> {
  purr(): void {}
}

/**
 * Abstract base for renderers.
 *
 * Provides the foundation for all rendering implementations. Subclasses
 * must implement the `render` method and the `canvas` getter.
 *
 * ## Performance
 *
 * The base class handles buffering and frame timing. Subclasses
 * should focus only on the actual rendering logic.
 *
 * @deprecated Use ConcreteRenderer instead.
 */
export abstract class AbstractRenderer {
  constructor(width: number, height: number) {}
  abstract render(input: string): string;
  abstract get canvas(): HTMLElement;
  format(input: string): string { return input; }
}

/** Serialization interface. */
export interface Serializable<T> {
  /**
   * Convert to JSON string.
   *
   * Serializes the object into a JSON string representation.
   * The output is always valid JSON that can be parsed back.
   */
  toJSON(): string;
  /** Parse from string. */
  fromString(input: string): T;
  readonly kind: string;
  optional?: boolean;
}

/** Extended serializable. */
export interface AdvancedSerializable<T> extends Serializable<T> {
  new(data: string): AdvancedSerializable<T>;
  toBinary(): Uint8Array;
  compress(level: number): Uint8Array;
}

/** An empty interface. */
export interface EmptyMarker {}

/** Interface with index signature. */
export interface StringMap {
  [key: string]: string;
}

/** Color values. */
export enum Color {
  Red = "red",
  Green = "green",
  Blue = "blue",
}

/** Log levels. */
export enum LogLevel {
  Debug,
  Info,
  Warn,
  Error,
}

/** Application config. */
export type Config<T = unknown> = {
  debug: boolean;
  verbose: boolean;
  settings: Record<string, T>;
};

/** Simple string alias. */
export type ID = string;

/**
 * Create the application.
 * @param name The app name.
 * @param config Optional config.
 */
export function createApp(name: string, config?: Partial<Config>): App;
export function createApp(config: Config): App;
export function createApp(nameOrConfig: string | Config, config?: Partial<Config>): App {
  return {} as App;
}

/**
 * Format a date to string.
 * @param date The date to format.
 */
export function formatDate(date: Date, locale?: string): string {
  return "";
}

/**
 * Validate input data.
 * @param data The data to validate.
 */
export function validate<T>(data: unknown, schema: Schema<T>): data is T {
  return true as any;
}

/** The current version. */
export const VERSION: string = "1.0.0";

/** Maximum retry count. */
export const MAX_RETRIES = 3;

export let mutableState: { count: number; label: string };

/**
 * A class-based parser.
 *
 * Parses input strings into structured data. The parser operates
 * synchronously and throws on invalid input.
 *
 * ## Error Handling
 *
 * The parser throws `ParseError` for malformed input.
 * Use `tryParse` for a non-throwing alternative.
 *
 * @example
 * ```ts
 * const p = new Parser();
 * p.parse("input");
 * ```
 */
export class Parser<T = string> {
  parse(input: string): T { return {} as T; }
  tryParse(input: string): T | null { return null; }
  static fromConfig(config: Config): Parser { return new Parser(); }
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
  process(): void;
}

export const CompoundType: { new(): CompoundType } = null as any;

/**
 * Logger utility.
 *
 * Logs messages to the console with optional log levels.
 * All messages are prefixed with a timestamp.
 *
 * ## Log Format
 *
 * ```
 * [TIMESTAMP] [LEVEL] message
 * ```
 */
export function log(message: string, level?: LogLevel): void {}

/** @deprecated Use log() instead. */
export function debugLog(message: string): void {}

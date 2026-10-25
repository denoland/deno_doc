/** The replacement context type. */
export interface FreshContext {
  state: unknown;
}

/**
 * @deprecated Use {@link FreshContext} instead
 */
export type PageProps = FreshContext;

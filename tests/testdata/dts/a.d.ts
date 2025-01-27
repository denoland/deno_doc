// Copyright 2018-2025 the Deno authors. MIT license.

/// <reference no-default-lib="true" />
/// <reference lib="esnext" />

interface WebSocket {
  readonly bufferedAmount: number;
}

declare var WebSocket: {
  readonly prototype: WebSocket;
  new (url: string, protocols?: string | string[]): WebSocket;
  readonly OPEN: number;
};

interface ResponseInit {
  status?: number;
  statusText?: string;
}


// Copyright 2020-2021 the Deno authors. All rights reserved. MIT license.
// @generated file from build script, do not edit
// deno-lint-ignore-file

let cachedTextDecoder = new TextDecoder("utf-8", {
  ignoreBOM: true,
  fatal: true,
});

cachedTextDecoder.decode();

let cachegetUint8Memory0 = null;
function getUint8Memory0() {
  if (
    cachegetUint8Memory0 === null ||
    cachegetUint8Memory0.buffer !== wasm.memory.buffer
  ) {
    cachegetUint8Memory0 = new Uint8Array(wasm.memory.buffer);
  }
  return cachegetUint8Memory0;
}

function getStringFromWasm0(ptr, len) {
  return cachedTextDecoder.decode(getUint8Memory0().subarray(ptr, ptr + len));
}

const heap = new Array(32).fill(undefined);

heap.push(undefined, null, true, false);

let heap_next = heap.length;

function addHeapObject(obj) {
  if (heap_next === heap.length) heap.push(heap.length + 1);
  const idx = heap_next;
  heap_next = heap[idx];

  heap[idx] = obj;
  return idx;
}

function getObject(idx) {
  return heap[idx];
}

let WASM_VECTOR_LEN = 0;

let cachedTextEncoder = new TextEncoder("utf-8");

const encodeString = function (arg, view) {
  return cachedTextEncoder.encodeInto(arg, view);
};

function passStringToWasm0(arg, malloc, realloc) {
  if (realloc === undefined) {
    const buf = cachedTextEncoder.encode(arg);
    const ptr = malloc(buf.length);
    getUint8Memory0().subarray(ptr, ptr + buf.length).set(buf);
    WASM_VECTOR_LEN = buf.length;
    return ptr;
  }

  let len = arg.length;
  let ptr = malloc(len);

  const mem = getUint8Memory0();

  let offset = 0;

  for (; offset < len; offset++) {
    const code = arg.charCodeAt(offset);
    if (code > 0x7F) break;
    mem[ptr + offset] = code;
  }

  if (offset !== len) {
    if (offset !== 0) {
      arg = arg.slice(offset);
    }
    ptr = realloc(ptr, len, len = offset + arg.length * 3);
    const view = getUint8Memory0().subarray(ptr + offset, ptr + len);
    const ret = encodeString(arg, view);

    offset += ret.written;
  }

  WASM_VECTOR_LEN = offset;
  return ptr;
}

let cachegetInt32Memory0 = null;
function getInt32Memory0() {
  if (
    cachegetInt32Memory0 === null ||
    cachegetInt32Memory0.buffer !== wasm.memory.buffer
  ) {
    cachegetInt32Memory0 = new Int32Array(wasm.memory.buffer);
  }
  return cachegetInt32Memory0;
}

function dropObject(idx) {
  if (idx < 36) return;
  heap[idx] = heap_next;
  heap_next = idx;
}

function takeObject(idx) {
  const ret = getObject(idx);
  dropObject(idx);
  return ret;
}

function debugString(val) {
  // primitive types
  const type = typeof val;
  if (type == "number" || type == "boolean" || val == null) {
    return `${val}`;
  }
  if (type == "string") {
    return `"${val}"`;
  }
  if (type == "symbol") {
    const description = val.description;
    if (description == null) {
      return "Symbol";
    } else {
      return `Symbol(${description})`;
    }
  }
  if (type == "function") {
    const name = val.name;
    if (typeof name == "string" && name.length > 0) {
      return `Function(${name})`;
    } else {
      return "Function";
    }
  }
  // objects
  if (Array.isArray(val)) {
    const length = val.length;
    let debug = "[";
    if (length > 0) {
      debug += debugString(val[0]);
    }
    for (let i = 1; i < length; i++) {
      debug += ", " + debugString(val[i]);
    }
    debug += "]";
    return debug;
  }
  // Test for built-in
  const builtInMatches = /\[object ([^\]]+)\]/.exec(toString.call(val));
  let className;
  if (builtInMatches.length > 1) {
    className = builtInMatches[1];
  } else {
    // Failed to match the standard '[object ClassName]'
    return toString.call(val);
  }
  if (className == "Object") {
    // we're a user defined class or Object
    // JSON.stringify avoids problems with cycles, and is generally much
    // easier than looping through ownProperties of `val`.
    try {
      return "Object(" + JSON.stringify(val) + ")";
    } catch (_) {
      return "Object";
    }
  }
  // errors
  if (val instanceof Error) {
    return `${val.name}: ${val.message}\n${val.stack}`;
  }
  // TODO we could test for more things here, like `Set`s and `Map`s.
  return className;
}

const CLOSURE_DTORS = new FinalizationRegistry((state) => {
  wasm.__wbindgen_export_2.get(state.dtor)(state.a, state.b);
});

function makeMutClosure(arg0, arg1, dtor, f) {
  const state = { a: arg0, b: arg1, cnt: 1, dtor };
  const real = (...args) => {
    // First up with a closure we increment the internal reference
    // count. This ensures that the Rust closure environment won't
    // be deallocated while we're invoking it.
    state.cnt++;
    const a = state.a;
    state.a = 0;
    try {
      return f(a, state.b, ...args);
    } finally {
      if (--state.cnt === 0) {
        wasm.__wbindgen_export_2.get(state.dtor)(a, state.b);
        CLOSURE_DTORS.unregister(state);
      } else {
        state.a = a;
      }
    }
  };
  real.original = state;
  CLOSURE_DTORS.register(real, state, state);
  return real;
}
function __wbg_adapter_16(arg0, arg1, arg2) {
  wasm
    ._dyn_core__ops__function__FnMut__A____Output___R_as_wasm_bindgen__closure__WasmClosure___describe__invoke__he70e61f338df988f(
      arg0,
      arg1,
      addHeapObject(arg2),
    );
}

function isLikeNone(x) {
  return x === undefined || x === null;
}
/**
* @param {string} root_specifier
* @param {Function} load
* @param {Function | undefined} maybe_resolve
* @returns {Promise<any>}
*/
export function doc(root_specifier, load, maybe_resolve) {
  var ptr0 = passStringToWasm0(
    root_specifier,
    wasm.__wbindgen_malloc,
    wasm.__wbindgen_realloc,
  );
  var len0 = WASM_VECTOR_LEN;
  var ret = wasm.doc(
    ptr0,
    len0,
    addHeapObject(load),
    isLikeNone(maybe_resolve) ? 0 : addHeapObject(maybe_resolve),
  );
  return takeObject(ret);
}

function handleError(f, args) {
  try {
    return f.apply(this, args);
  } catch (e) {
    wasm.__wbindgen_exn_store(addHeapObject(e));
  }
}
function __wbg_adapter_24(arg0, arg1, arg2, arg3) {
  wasm.wasm_bindgen__convert__closures__invoke2_mut__hd11990dd58c88e18(
    arg0,
    arg1,
    addHeapObject(arg2),
    addHeapObject(arg3),
  );
}

const imports = {
  __wbindgen_placeholder__: {
    __wbindgen_json_parse: function (arg0, arg1) {
      var ret = JSON.parse(getStringFromWasm0(arg0, arg1));
      return addHeapObject(ret);
    },
    __wbindgen_json_serialize: function (arg0, arg1) {
      const obj = getObject(arg1);
      var ret = JSON.stringify(obj === undefined ? null : obj);
      var ptr0 = passStringToWasm0(
        ret,
        wasm.__wbindgen_malloc,
        wasm.__wbindgen_realloc,
      );
      var len0 = WASM_VECTOR_LEN;
      getInt32Memory0()[arg0 / 4 + 1] = len0;
      getInt32Memory0()[arg0 / 4 + 0] = ptr0;
    },
    __wbindgen_object_drop_ref: function (arg0) {
      takeObject(arg0);
    },
    __wbindgen_string_new: function (arg0, arg1) {
      var ret = getStringFromWasm0(arg0, arg1);
      return addHeapObject(ret);
    },
    __wbindgen_cb_drop: function (arg0) {
      const obj = takeObject(arg0).original;
      if (obj.cnt-- == 1) {
        obj.a = 0;
        return true;
      }
      var ret = false;
      return ret;
    },
    __wbg_new_fd4e4431d3a298e3: function (arg0, arg1) {
      var ret = new Error(getStringFromWasm0(arg0, arg1));
      return addHeapObject(ret);
    },
    __wbg_call_6cdbeff3b536233f: function () {
      return handleError(function (arg0, arg1, arg2) {
        var ret = getObject(arg0).call(getObject(arg1), getObject(arg2));
        return addHeapObject(ret);
      }, arguments);
    },
    __wbg_call_bdf9779978ed6415: function () {
      return handleError(function (arg0, arg1, arg2, arg3) {
        var ret = getObject(arg0).call(
          getObject(arg1),
          getObject(arg2),
          getObject(arg3),
        );
        return addHeapObject(ret);
      }, arguments);
    },
    __wbg_new_31636366544febdf: function (arg0, arg1) {
      try {
        var state0 = { a: arg0, b: arg1 };
        var cb0 = (arg0, arg1) => {
          const a = state0.a;
          state0.a = 0;
          try {
            return __wbg_adapter_24(a, state0.b, arg0, arg1);
          } finally {
            state0.a = a;
          }
        };
        var ret = new Promise(cb0);
        return addHeapObject(ret);
      } finally {
        state0.a = state0.b = 0;
      }
    },
    __wbg_resolve_89251e936a5e00ac: function (arg0) {
      var ret = Promise.resolve(getObject(arg0));
      return addHeapObject(ret);
    },
    __wbg_then_fe720822c4da5711: function (arg0, arg1) {
      var ret = getObject(arg0).then(getObject(arg1));
      return addHeapObject(ret);
    },
    __wbg_then_f040b93e57f11d67: function (arg0, arg1, arg2) {
      var ret = getObject(arg0).then(getObject(arg1), getObject(arg2));
      return addHeapObject(ret);
    },
    __wbindgen_debug_string: function (arg0, arg1) {
      var ret = debugString(getObject(arg1));
      var ptr0 = passStringToWasm0(
        ret,
        wasm.__wbindgen_malloc,
        wasm.__wbindgen_realloc,
      );
      var len0 = WASM_VECTOR_LEN;
      getInt32Memory0()[arg0 / 4 + 1] = len0;
      getInt32Memory0()[arg0 / 4 + 0] = ptr0;
    },
    __wbindgen_throw: function (arg0, arg1) {
      throw new Error(getStringFromWasm0(arg0, arg1));
    },
    __wbindgen_closure_wrapper818: function (arg0, arg1, arg2) {
      var ret = makeMutClosure(arg0, arg1, 154, __wbg_adapter_16);
      return addHeapObject(ret);
    },
  },
};

const wasm_url = new URL("deno_doc_bg.wasm", import.meta.url);
let wasmInstantiatePromise;
switch (wasm_url.protocol) {
  case "file:": {
    if ("permissions" in Deno) {
      Deno.permissions.request({ name: "read", path: wasm_url });
    }
    const wasmCode = await Deno.readFile(wasm_url);
    wasmInstantiatePromise = WebAssembly.instantiate(wasmCode, imports);
    break;
  }
  case "https:":
  case "http:": {
    if ("permissions" in Deno) {
      Deno.permissions.request({ name: "net", host: wasm_url.host });
    }
    const wasmResponse = await fetch(wasm_url);
    if (
      wasmResponse.headers.get("content-type")?.toLowerCase().startsWith(
        "application/wasm",
      )
    ) {
      wasmInstantiatePromise = WebAssembly.instantiateStreaming(
        wasmResponse,
        imports,
      );
    } else {
      wasmInstantiatePromise = WebAssembly.instantiate(
        await wasmResponse.arrayBuffer(),
        imports,
      );
    }
    break;
  }
  default:
    throw new Error(`Unsupported protocol: ${wasm_url.protocol}`);
}

const wasmInstance = (await wasmInstantiatePromise).instance;
const wasm = wasmInstance.exports;

/* for testing and debugging */
export const _wasm = wasm;
export const _wasmInstance = wasmInstance;

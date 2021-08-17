#!/usr/bin/env -S deno run --unstable --allow-run --allow-read --allow-write --allow-env
// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

import * as colors from "https://deno.land/std@0.104.0/fmt/colors.ts";

await Deno.permissions.request({ name: "env" });
await Deno.permissions.request({ name: "run" });
await Deno.permissions.request({ name: "read" });
await Deno.permissions.request({ name: "write" });

console.log(
  `${colors.bold(colors.green("Building"))} deno_doc web assembly...`,
);

const home = Deno.env.get("HOME");
const root = new URL(".", import.meta.url).pathname;
const copyrightHeader = `// Copyright 2020-${
  new Date().getFullYear()
} the Deno authors. All rights reserved. MIT license.`;

if (new URL(import.meta.url).protocol === "file:") {
  Deno.chdir(root);
} else {
  console.error("The build script can only be run from a local file system");
  Deno.exit(1);
}

const cargoFmtCmd = ["cargo", "fmt"];
console.log(`  ${colors.bold(colors.gray(cargoFmtCmd.join(" ")))}`);
const cargoFmtCmdStatus = Deno.run({ cmd: cargoFmtCmd }).status();
if (!(await cargoFmtCmdStatus).success) {
  console.error(`cargo fmt failed`);
  Deno.exit(1);
}

const cargoBuildCmd = [
  "cargo",
  "build",
  "--release",
  "--no-default-features",
  "--features",
  "wasm",
  "--target",
  "wasm32-unknown-unknown",
];
console.log(`  ${colors.bold(colors.gray(cargoBuildCmd.join(" ")))}`);
const cargoBuildReleaseCmdStatus = Deno.run({
  cmd: cargoBuildCmd,
  env: {
    "SOURCE_DATE_EPOCH": "1600000000",
    "TZ": "UTC",
    "LC_ALL": "C",
    "RUSTFLAGS": `--remap-path-prefix=${root}=. --remap-path-prefix=${home}=~`,
  },
}).status();
if (!(await cargoBuildReleaseCmdStatus).success) {
  console.error(`cargo build failed`);
  Deno.exit(1);
}

const wasmBindGenCmd = [
  "wasm-bindgen",
  "./target/wasm32-unknown-unknown/release/deno_doc.wasm",
  "--target",
  "deno",
  "--weak-refs",
  "--out-dir",
  "./target/wasm32-bindgen-deno-js",
];
console.log(`  ${colors.bold(colors.gray(wasmBindGenCmd.join(" ")))}`);
const wasmBindgenCmdStatus = Deno.run({ cmd: wasmBindGenCmd }).status();
if (!(await wasmBindgenCmdStatus).success) {
  console.error(`wasm-bindgen failed`);
  Deno.exit(1);
}

console.log(
  `${colors.bold(colors.green("Copying"))} lib wasm...`,
);

const denoDocWasmDest = "./lib/deno_doc_bg.wasm";
await Deno.copyFile(
  "./target/wasm32-bindgen-deno-js/deno_doc_bg.wasm",
  denoDocWasmDest,
);
console.log(`  copy ${colors.yellow(denoDocWasmDest)}`);

console.log(
  `${colors.bold(colors.green("Generating"))} lib JS bindings...`,
);

const generatedJs = await Deno.readTextFile(
  "./target/wasm32-bindgen-deno-js/deno_doc.js",
);
const bindingJs = `${copyrightHeader}
// @generated file from build script, do not edit
// deno-lint-ignore-file

${
  generatedJs.replace(
    /^\s*wasmCode\s=\sawait Deno\.readFile\(wasm_url\);$/sm,
    `if ("permissions" in Deno) {\nDeno.permissions.request({ name: "read" });\n}\nwasmCode = await Deno.readFile(wasm_url);`,
  ).replace(
    /^\s*wasmCode\s=\sawait\s\(await\sfetch\(wasm_url\)\)\.arrayBuffer\(\);$/sm,
    `if ("permissions" in Deno) {\nDeno.permissions.request({ name: "net", host: wasm_url.host });\n}\nwasmCode = await (await fetch(wasm_url)).arrayBuffer();`,
  )
}

/* for testing and debugging */
export const _wasm = wasm;
export const _wasmInstance = wasmInstance;
`;
const libDenoDocJs = "./lib/deno_doc.js";
console.log(`  write ${colors.yellow(libDenoDocJs)}`);
await Deno.writeTextFile(libDenoDocJs, bindingJs);

const denoFmtCmd = [
  "deno",
  "fmt",
  "--quiet",
  "./lib/deno_doc.wasm.js",
  "./lib/deno_doc.js",
];
console.log(`  ${colors.bold(colors.gray(denoFmtCmd.join(" ")))}`);
const denoFmtCmdStatus = Deno.run({ cmd: denoFmtCmd }).status();
if (!(await denoFmtCmdStatus).success) {
  console.error("deno fmt command failed");
  Deno.exit(1);
}

console.log(
  `${colors.bold(colors.green("Finished"))} deno_doc web assembly.`,
);

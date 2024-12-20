import $ from "@david/dax";
import browserslist from "browserslist";
import { browserslistToTargets, transform } from "lightningcss";

const browsers = browserslist(">= 0.5%, not dead");

const styles =
  await $`deno run -A npm:tailwindcss@3.4.3 --input src/html/templates/styles.css`
    .text();
const stylesWrapped = ".ddoc {" + styles + "}";
const stylesFinal = transform({
  filename: "./styles.css",
  code: new TextEncoder().encode(stylesWrapped),
  minify: true,
  targets: browserslistToTargets(browsers),
  analyzeDependencies: false,
});
await Deno.writeFile("src/html/templates/styles.gen.css", stylesFinal.code);

const page =
  await $`deno run -A npm:tailwindcss@3.4.3 --config=./src/html/templates/pages/tailwind.config.ts --input src/html/templates/pages/page.css`
    .bytes();
const pageFinal = transform({
  filename: "./page.css",
  code: page,
  minify: true,
  targets: browserslistToTargets(browsers),
  analyzeDependencies: false,
});
await Deno.writeFile("src/html/templates/pages/page.gen.css", pageFinal.code);

const reset =
  await $`deno run -A npm:tailwindcss@3.4.3 --input src/html/templates/pages/reset.css`
    .bytes();
const resetFinal = transform({
  filename: "./reset.css",
  code: reset,
  minify: true,
  targets: browserslistToTargets(browsers),
  analyzeDependencies: false,
});
await Deno.writeFile("src/html/templates/pages/reset.gen.css", resetFinal.code);

const comrak =
  await $`deno run -A npm:tailwindcss@3.4.3 --input src/html/templates/comrak.css`
    .bytes();
const comrakFinal = transform({
  filename: "./comrak.css",
  code: comrak,
  minify: true,
  targets: browserslistToTargets(browsers),
  analyzeDependencies: false,
});
await Deno.writeFile("src/html/templates/comrak.gen.css", comrakFinal.code);

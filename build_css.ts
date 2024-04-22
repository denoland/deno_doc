import $ from "jsr:@david/dax@0.40.1";
import browserslist from "npm:browserslist@4.23.0";
import { browserslistToTargets, transform } from "npm:lightningcss";

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

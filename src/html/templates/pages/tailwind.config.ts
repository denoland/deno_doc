import { type Config } from "npm:tailwindcss@3.4.0";

export default {
  content: [
    "./src/html/templates/pages/*.hbs",
    "./src/html/templates/pages/search.js",
  ],
  darkMode: "class",
} as Config;

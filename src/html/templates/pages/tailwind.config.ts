import { type Config } from "npm:tailwindcss@3.4.3";

export default {
  darkMode: "class",
  theme: {
    extend: {
      colors: {
        backgroundDark: "#121417",
      },
    },
  },
  content: [
    "./src/html/templates/pages/*.hbs",
    "./src/html/templates/pages/search.js",
  ],
} as Config;

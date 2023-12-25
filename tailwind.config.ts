import { type Config } from "npm:tailwindcss@3.4.0";

export default {
  content: [
    "./src/html/**/*.rs",
    "./src/html/templates/*.hbs",
  ],
  safelist: [
    {
      pattern:
        /^(text|bg)-(Function|Variable|Class|Enum|Interface|TypeAlias|Namespace)(\/15)?$/,
    },
  ],
  theme: {
    extend: {
      colors: {
        "Function": "#056CF0",
        "Variable": "#7E57C0",
        "Class": "#20B44B",
        "Enum": "#22ABB0",
        "Interface": "#D2A064",
        "TypeAlias": "#A4478C",
        "Namespace": "#D25646",
      },
    },
  },
  darkMode: "class",
} as Config;

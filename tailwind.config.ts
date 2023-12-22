import { type Config } from "npm:tailwindcss@3.3.5";

export default {
  content: [
    "./src/html/**/*.rs",
    "./src/html/templates/*.html",
  ],
  safelist: [
    {
      pattern:
        /^(text|bg)-(Function|Variable|Class|Enum|Interface|TypeAlias|Namespace)-(text|bg)$/,
    },
  ],
  theme: {
    extend: {
      colors: {
        "Function": {
          text: "#056CF0",
          bg: "#026BEB1A",
        },
        "Variable": {
          text: "#7E57C0",
          bg: "#7E57C01A",
        },
        "Class": {
          text: "#20B44B",
          bg: "#2FA8501A",
        },
        "Enum": {
          text: "#22ABB0",
          bg: "#22ABB01A",
        },
        "Interface": {
          text: "#D2A064",
          bg: "#D4A0681A",
        },
        "TypeAlias": {
          text: "#A4478C",
          bg: "#A4478C1A",
        },
        "Namespace": {
          text: "#D25646",
          bg: "#D256461A",
        },
      },
    },
  },
  darkMode: "class",
} as Config;

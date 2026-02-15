import { type Config } from "tailwindcss";

const TAG_PURPLE = "#7B61FF";
const TAG_CYAN = "#0CAFC6";

const extraColors = {
  "Function": "#056CF0",
  "FunctionDark": "#4cc3ff",
  "Method": "#056CF0",
  "MethodDark": "#4cc3ff",
  "Variable": "#7E57C0",
  "VariableDark": "#b37feb",
  "Property": "#7E57C0",
  "PropertyDark": "#b37feb",
  "Class": "#20B44B",
  "ClassDark": "#87eea4",
  "Enum": "#22ABB0",
  "EnumDark": "#3fced1",
  "Interface": "#D2A064",
  "InterfaceDark": "#bb733b",
  "TypeAlias": "#A4478C",
  "TypeAliasDark": "#dd95cc",
  "Namespace": "#D25646",
  "NamespaceDark": "#e57e6b",

  "new": TAG_PURPLE,
  "abstract": TAG_CYAN,
  "deprecated": "#DC2626", // red 600
  "unstable": TAG_PURPLE,
  "writeonly": TAG_PURPLE,
  "readonly": TAG_PURPLE,
  "protected": TAG_PURPLE,
  "private": TAG_CYAN,
  "optional": TAG_CYAN,
  "permissions": TAG_CYAN,
  "other": "#57534E", // stone 600
};

export default {
  content: [
    "./src/html/**/*.rs",
    "./src/html/templates/*.hbs",
  ],
  darkMode: "class",
  safelist: [
    {
      pattern: new RegExp(`^text-(${Object.keys(extraColors).join("|")})$`),
      variants: ["dark"],
    },
    {
      pattern: new RegExp(
        `^border-(${Object.keys(extraColors).join("|")})\/50$`,
      ),
    },
    {
      pattern: new RegExp(
        `^bg-(${Object.keys(extraColors).join("|")})\/(?:5|15)$`,
      ),
      variants: ["hover", "dark"],
    },
  ],
  theme: {
    extend: {
      colors: {
        ...extraColors,
        "contextLink": "#0E6590",
        "contextLinkDark": "#86d3f3",
        backgroundDark: "#121417",
        gray: {
          1: "#868789",
        },
      },
      borderWidth: {
        "1.5": "1.5px",
      },
      margin: {
        "indent": "2ch",
      },
      maxWidth: {
        "prose": "75ch",
      },
    },
  },
} as Config;

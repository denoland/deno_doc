import { type Config } from "npm:tailwindcss@3.3.5";

export default {
  content: [
    "./src/html/**/*.{html,rs}",
  ],
  theme: {
    extend: {
      colors: {
        default: "#12124B", // should be neutral-800, waiting for LP redesign

        azure3: "#E1ECF2",
        mainBlue: "#0094FF",
        normalBlue: "#0A4BAB",
        azure: "#E1F8FF",
        azure2: "#D2DDE2",

        colorWashFrom: "#002585",
        colorWashTo: "#209DEE",
        white: "#ffffff",

        // New design system

        black: "#0B0D11",
        offblack: "#121417",

        runtime: "#70FFAF",
        "runtime-dark": "#172723",
        "runtime-secondary": "#EBFF01",
        "runtime-secondary-dark": "#232711",

        deploy: "#01C2FF",
        "deploy-dark": "#0C212A",

        subhosting: "#FF8A01",
        "subhosting-dark": "#251C11",

        gray: {
          1: "#868789",
          2: "#56575A",
          3: "#25272B",
          4: "#191B1F",
          5: "#14161A",
        },

        code: {
          1: "#01C2FF",
          2: "#00A341",
          3: "#AE01FF",
          4: "#EA8E04",
          5: "#FFD601",
        },
      },
      fontFamily: {
        sans:
          "Inter,ui-sans-serif,system-ui,-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Helvetica Neue,Arial,Noto Sans,sans-serif",
        mono: [
          "Menlo",
          "Monaco",
          "Lucida Console",
          "Consolas",
          "Liberation Mono",
          "Courier New",
          "monospace",
        ],
      },
      spacing: {
        1.75: "0.4375rem",
        4.5: "1.125rem",
        5.5: "1.375rem",
        15: "3.75rem",
        18: "4.5rem",
        22: "5.5rem",
        72: "18rem",
        76: "19rem",
        88: "22rem",
        136: "34rem",
        208: "52rem",
      },
      fontSize: {
        "1.5xl": ["1.375rem", {
          lineHeight: "1.55rem",
        }],
        "2.5xl": ["1.75rem", {
          lineHeight: "2rem",
        }],
        "3.5xl": ["2rem", {
          lineHeight: "2.2rem",
        }],
        "4.5xl": ["2.75rem", {
          lineHeight: "3rem",
        }],
        "5.5xl": ["3.5rem", {
          lineHeight: "1",
        }],
        "6.5xl": ["4rem", {
          lineHeight: "1",
        }],
      },
      keyframes: {
        move: {
          "from": {
            transform: "translateX(-200px)",
          },
          "to": {
            transform: "translateX(100vw)",
          },
        },
        buttonBorderSpin: {
          "0%": { "--border-gradient-angle": "0turn" },
          "100%": { "--border-gradient-angle": "1turn" },
        },
        badgeBorderSpin: {
          "0%": { "--border-gradient-angle": "0turn" },
          "100%": { "--border-gradient-angle": "-1turn" },
        },
        slide_in_up: {
          "from": { transform: "translateY(0.65em)", opacity: 0 },
          "to": { transform: "translateY(0)", opacity: 1 },
        },
      },
      animation: {
        move: "move 6s linear infinite",
        "button-border-spin":
          "buttonBorderSpin 1.2s cubic-bezier(0.52, 0.2, 0.43, 1.0) forwards",
        "badge-border-spin": "badgeBorderSpin 10s linear infinite",
        "header-nav-item-slide":
          "slide_in_up 0.25s cubic-bezier(0, 0.37, 0.17, 1) forwards",
      },
      gridTemplateColumns: {
        footer: "auto 1fr auto",
      },
    },
  },
  darkMode: "class",
} as Config;

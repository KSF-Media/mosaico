const plugin = require("tailwindcss/plugin");

const sansFonts = [
  "ui-sans-serif",
  "system-ui",
  "-apple-system",
  "BlinkMacSystemFont",
  '"Segoe UI"',
  "Roboto",
  '"Helvetica Neue"',
  "Arial",
  '"Noto Sans"',
  "sans-serif",
  '"Apple Color Emoji"',
  '"Segoe UI Emoji"',
  '"Segoe UI Symbol"',
  '"Noto Color Emoji"',
];

const serifFonts = ["ui-serif", "Georgia", "Cambria", '"Times New Roman"', "Times", "serif"];

const monoFonts = [
  "ui-monospace",
  "SFMono-Regular",
  "Menlo",
  "Monaco",
  "Consolas",
  '"Liberation Mono"',
  '"Courier New"',
  "monospace",
];

const listOfIcons = {
  hbl: `url(../images/logo-hbl.svg)`,
  vn: `url(../images/logo-vn.svg)`,
  on: `url(../images/logo-on.svg)`,
  search: `url("../images/icon-mosaico-search.svg")`,
  epaper: `url("../images/icon-mosaico-epaper.svg")`,
  crosswords: `url("../images/icon-mosaico-crossword.svg")`,
  kundservice: `url("../images/icon-mosaico-kundservice.svg")`,
  logout: `url("../images/icon-mosaico-logout.svg")`,
  login: `url("../images/icon-mosaico-login.svg")`,
};

const maskImagePlugin = plugin(
  function ({ matchUtilities, theme }) {
    matchUtilities(
      {
        maskimage: (value) => ({
          maskImage: value,
          maskPosition: "0 0",
        }),
      },
      { values: theme("maskImage") }
    );
  },

  {
    theme: {
      maskImage: listOfIcons,
    },
  }
);

const maskSizePlugin = plugin(function ({ matchUtilities, theme }) {
  matchUtilities(
    {
      "mask-size": (value) => ({
        maskSize: `${value} ${value}`,
      }),
    },
    { values: theme("spacing") }
  );
});

module.exports = {
  content: {
    files: ["./src/**/*", "./static/**/*"],
    transform: {
      // We have to use \\_ instead of \_ in purs files
      purs: (content) => content.replace(/\\\\/g, "\\"),
    },
  },
  theme: {
    extend: {
      spacing: {
        192: "768px", // article title
        216: "864px", // article body
        240: "960px", // article image
      },
    },
    colors: {
      transparent: "transparent",
      gray: {
        50: "#f7f5f3", // light
        75: "#eceae6", // nude
        100: "#e0e0e0", // hairline-color
        200: "#dcd9d7", // almostlight
        300: "#cbcaca", // slightlylight
        400: "#999999", // mediumlight
        500: "#6d7573", // medium
        600: "#575d5c", // mediumdark
        700: "#535251", // warm-mid
        800: "#333333", // dark
        900: "#141414", // dark-text
        950: "#0f1011", // deepdark
      },
      white: "#ffffff",
      black: "#000000",
      blue: {
        link: "#00698e",
      },
      green: {
        300: "#249c25", // button-color-primary-hover
        500: "#00810a", // button-color-primary
      },
      hbl: "#f07e26",
      on: "#518196",
      vn: "#c90c0f",
      neutral: "#00a1ab",
      brand: "var(--brand-color)", // defined in _site.scss
      advertorial: "var(--color-advertorial, #fff3e6)", // defined in Aptoma's CSS
    },

    fontFamily: {
      roboto: ["Roboto", ...sansFonts],
      robotoslab: ["Roboto Slab", ...sansFonts],
      duplexsans: ['"Duplex Sans"', ...sansFonts],
      duplexserif: ['"Duplex Serif"', ...serifFonts],
      mono: ["ui-monospace", ...monoFonts],
    },

    screens: {
      sm: "320px", // @content-block
      md: "760px", // @breakpoint-2col
      lg: "1020px", // @breakpoint-3col
    },
    backgroundImage: listOfIcons,
  },
  plugins: [maskImagePlugin, maskSizePlugin],
};

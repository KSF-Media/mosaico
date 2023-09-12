import * as esbuild from "esbuild";
import babel from "@babel/core";
import { lessLoader } from "esbuild-plugin-less";
import { config } from "dotenv";
config();
import glob from "tiny-glob";

import axios from "axios";
import path from "path";
import { sassPlugin } from "esbuild-sass-plugin";
import postcss from "postcss";
import autoprefixer from "autoprefixer";
import tailwindcss from "tailwindcss";

import * as fs from "fs";
import cheerio from "cheerio";
import * as util from "util";
import * as cp from "child_process";

const exec = util.promisify(cp.exec);
const writeFile = util.promisify(fs.writeFile);
const readFile = util.promisify(fs.readFile);

const minify = process.env.NODE_ENV === "production";

const plugins = [
  lessLoader(),
  sassPlugin({
    async transform(source, resolveDir) {
      const { css } = await postcss()
        .use(autoprefixer)
        .use(tailwindcss(path.resolve("./tailwind.config.js")))
        .process(source, {
          from: "./src/_site.scss",
        });
      return css;
    },
  }),
];

export async function runBuild() {
  try {
    console.log("Bundling javascript...");

    const templateFile = "./web/index.html";
    const template = cheerio.load(fs.readFileSync(templateFile, "utf8"));

    const buildOpts = {
      entryPoints: ["./web/index.js", "./web/darkmode.js"],
      entryNames: "[name]-[hash]",
      bundle: true,
      outdir: "./dist/assets",
      minify,
      metafile: true,
      plugins,
      loader: {
        ".js": "jsx",
        ".png": "file",
        ".woff": "file",
        ".ttf": "file",
        ".svg": "file",
        ".otf": "file",
        ".eot": "file",
        ".woff2": "file",
        ".gif": "file",
        ".html": "file",
      },
      define: {
        "process.env.BOTTEGA_URL": '"' + process.env.BOTTEGA_URL + '"',
        "process.env.LETTERA_URL": '"' + process.env.LETTERA_URL + '"',
        "process.env.PERSONA_URL": '"' + process.env.PERSONA_URL + '"',
        "process.env.PUBLIC_URL": '"' + process.env.PUBLIC_URL + '"',
        "process.env.INSECURE_COOKIE": '"' + process.env.INSECURE_COOKIE + '"',
        "process.env.FACEBOOK_APP_ID": '"' + process.env.FACEBOOK_APP_ID + '"',
        "process.env.GOOGLE_CLIENT_ID": '"' + process.env.GOOGLE_CLIENT_ID + '"',
        "process.env.PAPER": '"' + process.env.PAPER + '"',
        "process.env.HIDE_LOGIN_LINKS": '"' + process.env.HIDE_LOGIN_LINKS + '"',
        "process.env.SENTRY_DSN": '"' + process.env.SENTRY_DSN + '"',
        "process.env.JANRAIN_FLOW_VERSION": '"' + process.env.JANRAIN_FLOW_VERSION + '"',
        "process.env.JANRAIN_LOGIN_CLIENT_ID": '"' + process.env.JANRAIN_LOGIN_CLIENT_ID + '"',
        "process.env.JANRAIN_SSO_SERVER": '"' + process.env.JANRAIN_SSO_SERVER + '"',
        "process.env.JANRAIN_XD_RECEIVER_PATH": '"' + process.env.JANRAIN_XD_RECEIVER_PATH + '"',
      },
      treeShaking: true,
    };

    const result = await esbuild.build(buildOpts);
    // Refer to assets according to PUBLIC_URL env var
    // This will replace src or href of script and link tags
    let outfiles = Object.keys(result.metafile.outputs);

    // fetch latest Aptoma css url and insert href into template
    await axios
      .get(process.env.APTOMA_ASSETS)
      .then((response) => {
        template("#aptoma-css").attr("href", response.data.cssUrl);
      })
      .catch((err) => console.error("Error fetching Aptoma assets: " + err));

    const cookiebotDataId = {
      hbl: "e3464008-80f1-479f-99f2-dc8b41cd79a7",
      vn: "9aea18f0-286d-4988-af88-351c5bf83a2c",
      on: "c7d5e93e-e01c-417d-a1e8-f0ee495e0182",
    };
    template("#Cookiebot").attr("data-cbid", cookiebotDataId[process.env.PAPER]);

    template(".mosaico-asset").each((ix, elem) => {
      const src = template(elem).attr("src");
      const href = template(elem).attr("href");

      const assetName = src || href;
      const asset = assetName.split(".");

      // Makes a regex e.g. "/index-\w+\.js/"
      // The esbuild generated files are named like "index-EDGJWUC6.js"
      const assetRegex = new RegExp(asset[0] + "-\\w+\\." + asset[1]);
      const assetPath = outfiles.reduce((acc, fileName) => {
        const matchingPath = fileName.match(assetRegex);
        return matchingPath ? matchingPath[0] : acc;
      }, null);

      const publicUrl = process.env.PUBLIC_URL || "";
      if (src) {
        template(elem).attr("src", publicUrl + "/assets/" + assetPath);
      } else if (href) {
        template(elem).attr("href", publicUrl + "/assets/" + assetPath);
      }
    });
    const staticEntryPoints = await glob("./static/**/*.js");
    const staticBuildOpts = {
      entryPoints: staticEntryPoints,
      bundle: true,
      outdir: "./dist/static",
      minify,
      plugins,
      treeShaking: true,
      metafile: true,
    };

    /* The static page building is a three-step process:
       1) Copy everything from ./static to ./dist/static
          Essentially this copies the .html files as-is.
       2) Compile javascript from ./static to ./dist/static
          This allows using modern JS features such as modules.
          Server-side rendering serves static pages from ./dist/static.
       3) Copy everything from ./dist/static to ./dist/assets
          The browser static pages are loaded from ./dist/assets.
     */

    await exec("mkdir -p dist/static && cp -R ./static/* ./dist/static/");
    const staticResult = await esbuild.build(staticBuildOpts);
    await exec("mkdir -p dist/assets && cp -R ./dist/static/* ./dist/assets/");

    const staticFiles = Object.keys(staticResult.metafile.outputs);

    console.log("Transforming CSS files using Autoprefixer");
    await Promise.all(
      outfiles.map((file) =>
        file.endsWith(".css")
          ? readFile(file).then((css) =>
              postcss([autoprefixer])
                .process(css, { from: file, to: file })
                .then(async (result) => {
                  await writeFile(file, result.css);
                  if (result.map) {
                    await writeFile(file + ".map", result.map.toString());
                  }
                })
            )
          : Promise.resolve(undefined)
      )
    );

    if (minify) {
      console.log("Transpiling results with Babel");

      const babelOpts = {
        presets: [
          [
            "@babel/preset-env",
            {
              modules: false,
              useBuiltIns: "usage",
              corejs: "3.25.2",
            },
          ],
          "@babel/preset-react",
        ],
      };

      const babelTranspile = {
        presets: [
          [
            "@babel/preset-env",
            {
              modules: false,
              useBuiltIns: false,
            },
          ],
        ],
      };

      await Promise.all(
        [...outfiles, ...staticFiles, ...staticFiles.map(file => file.replace("dist/static", "dist/assets"))].map((file) =>
          file.endsWith(".js")
            ? babel
                // Add all required polyfills
                .transformFileAsync(file, babelOpts)
                // Transpile the imports
                .then(async ({ code }) => {
                  await writeFile(file, code);
                  const { entryPoints, entryNames, outdir, ...rest } = buildOpts;
                  // NB. the 'target: "es5"' here doesn't do transpiling, it just prevents esbuild from adding es6+ features
                  const postBuildTransform = { ...rest, entryPoints: [file], outfile: file, allowOverwrite: true, target: "es5" };
                  return esbuild.build(postBuildTransform);
                })
                .then(() => babel.transformFileAsync(file, babelTranspile))
                .then(({ code }) => writeFile(file, code))
            : Promise.resolve(undefined)
        )
      );
    }

    await exec("mkdir -p dist/ && cp ./redir/redir.json ./dist/");

    await writeFile("./dist/index.html", template.html());
    console.log("Wrote index.html");
  } catch (e) {
    console.warn("Build error", e);
  }
}

# Mosaico

The new and shiny sites!

It's a server side rendered react thing. The idea is that the first pageload will be returned from the server and after that it acts like a single page app.

To run things:
```
yarn install
yarn build
yarn start
```

## Development

We can mentally divide Mosaico into two parts: the server code and the browser code. In production, both parts are required in order to run it as designed. However, as building the server dependent parts and restarting the thing continuously takes time and requires patience, it might be desired to run only the browser part when developing Mosaico. That is, if the development work does not concern the server itself. You can start the development server using Browsersync with `yarn start-dev`.

`yarn start-dev` will run two parallel jobs:
```
# Watches for purescript changes
spago build --watch

# Runs Browsersync and hot-reloads browser on ./dist and ./web changes
# On ./web changes, runs esbuild too
node ./run/hot-reload.js
```
Basically we are watching for purescript code changes and bundled javascript changes and syncing those to the browser.

When developing the server side bits, you need to restart the server after any changes with `yarn start` or `spago run`. Note that if your server side code requires also changes to the browser side of things, you need to build the static files with esbuild before running your server. This is what `yarn build` does. This is a bit clumsy and again, a bit time consuming. The smoothest way of doing this currently is to run these commands (related [Spago issue](https://github.com/purescript/spago/issues/506)):
```
$ spago run
$ yarn start-dev
```


Under `web/` we have `index.html` and `index.js` which can be thought of as templates that we load in the browser. This is the entry point for Mosaico react component. In fact, with these files alone, one could run a single page app version of Mosaico (`yarn start-dev`). When the node server of Mosaico is involved, however, the server might want to write something to the files. Or more specifically, it will write to the esbuild built version of these files, located under `dist/`. This is what `yarn build` will do. Each yarn command is defined in `package.json` under `scripts` object. Let's look at what `yarn build` does:

```
"scripts": {
  "yarn build": "yarn run build-app && yarn run build-spa",
  ...
}
```

First we run `spago build` (`yarn run build-app`) which compiles our PureScript code into `output/`. This is important, as we use the compiled PureScript in `web/index.js`

```
...
var Mosaico = require("../output/Mosaico/index.js").jsApp();
...

```

After that, we run `node -e 'require(\"./run/build\").runBuild()` (`yarn run build-spa`). Here, we build the file `index.js` we have under `web/` into a destination directory `dist/assets/`. Esbuild does its thing: it finds every dependency it needs and places them into `dist/assets/`. Unlike Parcel, esbuild will not handle html files automatically. In stead, in `run/build.js` we manually copy `web/index.html` and our static pages from `static/*` into `dist/`.

On static pages, the app initialization expects the selector "#app .mosaico--static-content" to match with the element containing the static content.  This assumption isn't checked at build time.

### Pubsub

To test pubsub frontpage updates, create a service account key to
ksf-dev with Pub/Sub Editor and Pub/Sub Subscriber roles.  Just
Subscriber is not enough since it creates a new subscription at start
up time.

```
export GOOGLE_APPLICATION_CREDENTIALS=
```

Where credentials is the JSON file downloaded from Google's cloud.

### Server / Client

Mosaico uses the same code base for running a version of the site on
the front end and the server.  The differences this makes when
developing for either are mostly papered over, but some things may
still require a bit extra care.

#### Unified dependencies

Both environments use the same `package.json` and `spago.dhall` files
for defining dependencies.  This means that the deps have some
libraries that are to be used only in one context, like pubsub and the
Affjax driver packages.  This works as long as modules using those are
not included in both environments.  If a server only module is
imported in both contextes it will fail with a bundler error.

#### Environment variables

Environment variables aren't by their nature available on the client
side.  If you add new variables or modify their names, also update
`runBuild` in `run/build.mjs`.  It pretty much does string
substitutions if the JS files have matches with those.

If you go "wait, `process` is undefined in this context" (like in
`Mosaico.js`) you are not wrong, but the build has a trick for it.

## Testing ads and consent popups

The ads and consent modals don't work in localhost. In order to get them
working for local development, you need something like mitmproxy for
connections:

1) Install mitmproxy:
    - OS X: `$ brew install mitmproxy`
    - Linux: open browser & go to https://mitmproxy.org/, click download and move binaries to somewhere in `$PATH`
2) Add `127.0.0.1 mosaico-hbl.staging.ksfmedia.fi` to /etc/hosts
    - Or `127.0.0.1 www.hbl.fi` if you want to fake prod
3) Clear HSTS cache, otherwise you get HSTS errors:
    - Firefox: Open history, find the stage/prod hbl site, right click -> forget about this site
    - Chrome: navigate to `chrome://net-internals/#hsts`, enter the domain and click 'delete domain'
4) Start mosaico (if not running already)
5) `sudo mitmproxy -s scripts/cors.py --mode reverse:http://localhost:8000 -p 443`
6) Navigate to mosaico-hbl.staging.ksfmedia.fi and accept the self-signed certificate

## Testing local Mosaico with a phone

Ensure that your phone and laptop are in the same wifi.

1) Install mitmproxy (see above)
2) `yarn build`
3) Find out your local ip address (eg. 192.168.1.126)
4) Create a file called `proxy.pac` in `dist/`:
    - Content: `function FindProxyForURL(url, host) { return "SOCKS 192.168.1.126:8080"; }`
    - Replace the IP with your local ip
5) `yarn start`
6) `mitmproxy --mode socks5 -s scripts/proxy.py`
7) Configure your phone to use a automatic proxy script, and set that to `http://192.168.1.126:8080/proxy.pac` (replacing your IP again)
8) Open browser in your phone and go to `http://mitm.it`
9) Install certificate to your phone
10) `http://192.168.1.126:8000` should be your local mosaico

## Tests

Launch site as described in the Development sections.  The tests may
fail with ads, consider using `export DISABLE_ADS=1` before launching
Mosaico if that happens.

The test expects to get account data for testing from environment
variables.

```
export TEST_USER=
export TEST_PASSWORD=
export ENTITLED_USER=
export ENTITLED_PASSWORD=
export LETTERA_URL=
```

```
spago -x test.dhall test
```

## Static Pages
Since the js script does not work if inserted as inner html, therefore the script is in an external file. The js file has identical name as the html file and fetched at the same time as the html file and run after the DOM tree is built. When developing, `yarn start-dev` will include the static files and hot-reload changes in the browser. Both html and js files for static pages are found in `static/` directory.


## Git Hooks
You can install included git hooks with `make hooks`

## setTimeout(..., 5000) waits for only 500 milliseconds
Due to Google Tag Manager related reasons (see index.js), setTimeout is shimmed
to replace setTimeout calls with 5 seconds to setTimeout with only 500
milliseconds.

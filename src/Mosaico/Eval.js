export async function evalExternalScriptsImpl(scripts) {
  if (typeof document !== "undefined") {
    // Later scripts might depend on earlier scripts being executed,
    // so we can't parallelize this
    for (const script of scripts) {
      var dummy = document.createElement("div");
      dummy.innerHTML = script.trim();
      const scriptSrc = dummy.firstChild.getAttribute("src");
      // Do note that if any of the remote platforms gets pwned (eg. platform shuts down
      // and gets taken over by some domain squatter), we'll get some very nice self XSS here...
      if (scriptSrc?.indexOf("instagram.com") >= 0) {
        const script = document.createElement("script");
        script.src = scriptSrc;
        script.async = true;
        document.body.appendChild(script);
      } else if (scriptSrc) {
        const proxiedUrl = "/corsProxy?url=" + encodeURIComponent(scriptSrc);
        await fetch(proxiedUrl)
          .then((r) => r.text())
          .then(evalScript);
      } else {
        evalScript(dummy.firstChild.innerHTML);
      }
    }
  }
}

function evalScript(s) {
  try {
    var elem = document.createElement("script");
    elem.innerHTML = s;
    document.head.appendChild(elem);
  } catch (err) {
    console.warn("Failed to eval script:", err);
  }
}

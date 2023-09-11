function areAdsAllowed() {
  const interval = setInterval(() => {
    if (window.Cookiebot !== undefined) {
      setConsent();
      clearInterval(interval);
    } else {
      console.log("Could not find Cookiebot, trying again")
    }
  }, 500);

  function setConsent() {
    if (window.Cookiebot.consent.marketing) {
      window.consentToEmbeddedScripts(true);
      console.log("User has consented to ads cookies; allowing external scripts.");
    } else if (isDraft()) {
      window.consentToEmbeddedScripts(true);
      console.log("Draft preview; allow external scripts.");
    } else {
      window.consentToEmbeddedScripts(false);
      console.log("User has not consented to ads cookie; external scripts blocked.");
    }
  }
}

if (typeof window !== "undefined") {
  window.consentToEmbeddedScripts = (value) => {
    window.consentToEmbeddedScriptsResolve(value);
    consentedToEmbeddedScripts.then((consentStatus) => {
      if (!consentStatus && value) {
        // User has previously denied consent, but decided to now give it.
        // Refresh the page to load additional content.
        // Cannot use consentStatus !== value because the google consent box
        // will actually call this with the value of false about two seconds
        // after loading, regardless of the given consent.
        location.reload();
      }
    });
  };
  areAdsAllowed();
}

export const consentedToEmbeddedScripts =
  typeof window !== "undefined" &&
  new Promise((resolve) => {
    window.consentToEmbeddedScriptsResolve = resolve;
    if (isDraft()) {
      // We're in Aptoma's preview window, load embeds
      window.consentToEmbeddedScripts(true);
    }
    if (window.location.hostname === "localhost") {
      // We're on localhost, load embeds
      window.consentToEmbeddedScripts(true);
    }
  });

if (typeof window !== "undefined") {
  window.consentedToEmbeddedScripts = consentedToEmbeddedScripts;
}

export async function forceEvalExternalScriptsImpl(scripts) {
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

export async function evalExternalScriptsImpl(scripts) {
  if ((await consentedToEmbeddedScripts) == true || isDraft()) {
    forceEvalExternalScriptsImpl(scripts);
  } else {
    console.log("User has not consented to receive ads; not loading external scripts.");
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

function isDraft() {
  // return true for preview paths, starting with "/artikel/draft"
  // or domains "mosaico-<brand>.api" where Cookiebot isn't available
  return document && document.location.pathname.startsWith("/artikel/draft/") ||
         window && window.location.host.match(/^mosaico-(.*).api/);
}

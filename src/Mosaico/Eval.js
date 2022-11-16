/* This function will block until the funding choices API is loaded
   -- meaning that it will never resolve if the FC loading is blocked! */
function areAdsAllowed() {
  window.googlefc = window.googlefc || {};
  window.googlefc.ccpa = window.googlefc.ccpa || {};
  window.googlefc.callbackQueue = window.googlefc.callbackQueue || [];

  window.googlefc.callbackQueue.push({
    AD_BLOCK_DATA_READY: () => {
      if (window.googlefc.getAllowAdsStatus() === googlefc.AllowAdsStatusEnum.ADS_ALLOWED) {
        console.log("User has consented to ads; allowing external scripts.");
        window.consentToEmbeddedScripts(true);
      } else {
        window.consentToEmbeddedScripts(false);
      }
    },
  });
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
}

export const consentedToEmbeddedScripts = (typeof window !== "undefined") && new Promise(resolve => {
  window.consentToEmbeddedScriptsResolve = resolve;
  if(document && document.location.pathname.startsWith("/artikel/draft/")) {
    // We're in Aptoma's preview window, load embeds
    window.consentToEmbeddedScripts(true);
  }
});

export async function evalExternalScriptsImpl(scripts) {
  areAdsAllowed();
  if ((await consentedToEmbeddedScripts) == true) {
    if (typeof document !== "undefined") {
      // Later scripts might depend on earlier scripts being executed,
      // so we can't parallelize this
      for (const script of scripts) {
        var dummy = document.createElement("div");
        dummy.innerHTML = script.trim();
        const scriptSrc = dummy.firstChild.getAttribute("src");
        // Do note that if any of the remote platforms gets pwned (eg. platform shuts down
        // and gets taken over by some domain squatter), we'll get some very nice self XSS here...
        if (scriptSrc) {
          const proxiedUrl = "/corsProxy?url=" + encodeURIComponent(scriptSrc);
          await fetch(proxiedUrl)
            .then((r) => r.text())
            .then(evalScript);
        } else {
          evalScript(dummy.firstChild.innerHTML);
        }
      }
    }
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

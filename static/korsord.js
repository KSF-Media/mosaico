if (typeof window !== "undefined") {
  /* Wait for the main mosaico script to be loaded, which
   * defines the consentedToEmbeddedScripts promise */
  const interval = setInterval(() => {
    if (window.consentedToEmbeddedScripts && document.getElementById("keesing-crosswords")) {
      clearInterval(interval);

      window.consentedToEmbeddedScripts.then(() => {
        var wrapper = document.getElementById("keesing-crosswords");

        var cwDiv = document.createElement("div");
        cwDiv.setAttribute("id", "puzzle-portal");
        cwDiv.setAttribute("data-customerid", "ksf");
        cwDiv.setAttribute("data-publicpath", "https://web.keesing.com/pub/portal/v2.21.3/dist/");

        var cwScript = document.createElement("script");
        cwScript.src = "https://web.keesing.com/pub/portal/v2.21.3/dist/main-bundle.js";
        cwScript.type = "text/javascript";

        wrapper.appendChild(cwDiv);
        wrapper.appendChild(cwScript);
      });
    }
  }, 500);
}

if (typeof window !== "undefined") {
  const cwStyleClass = "korsord-print";
  /* Wait for the keesing script to be loaded */
  const interval = setInterval(() => {
    if (document.getElementById("keesing-crosswords")) {
      clearInterval(interval);

      const wrapper = document.getElementById("keesing-crosswords");

      const cwDiv = document.createElement("div");
      cwDiv.setAttribute("id", "puzzle-portal");
      cwDiv.setAttribute("data-customerid", "ksf");
      cwDiv.setAttribute("data-publicpath", "https://web.keesing.com/pub/portal/v2.21.3/dist/");

      const cwScript = document.createElement("script");
      cwScript.src = "https://web.keesing.com/pub/portal/v2.21.3/dist/main-bundle.js";
      cwScript.type = "text/javascript";

      wrapper.appendChild(cwDiv);
      wrapper.appendChild(cwScript);

      let bodyPrintArea;

      function beforePrint() {
        /* If the user navigates away from the korsord page and back, this script gets rerun. If the document
         * doesn't contain the cwDiv we are holding, this handler is not the correct one, so remove it.
         */
        if (document.contains(cwDiv)) {
          const crossword = document.querySelector("#puzzle-portal")?.cloneNode(true);
          if (crossword && document.querySelector("#puzzle-player")) {
            document.documentElement.classList.add(cwStyleClass);

            /* Reset possible zoom  */
            crossword.querySelectorAll(".zoom-wrapper").forEach((elem) => (elem.style = ""));

            /* Remove any selected cell highlighting */
            crossword.querySelectorAll(".grid-cell").forEach((elem) => {
              /* krypto squares might contain .grid-cell--black */
              if (elem.classList.contains("grid-cell--black")) {
                elem.setAttribute("class", "grid-cell grid-cell--black");
              } else {
                elem.setAttribute("class", "grid-cell");
              }
            });

            /* Remove any selected hint highlighting */
            crossword.querySelectorAll(".clues__item").forEach((elem) => {
              elem.classList.remove("clues__item--selected");
            });

            if (crossword.querySelector(".tectonic, .kdoku")) {
              /* tectonic/kdoku have some cell borders which are too dim for prints
                       (and to be honest, for web as well...) Extra fun round: the
                       thick borders are specified in inline styles and are puzzle-specific. */
              let thickStyle = crossword.querySelector(".grid-cell__border").style["stroke-width"];

              // The inline style with biggest stroke-width is the one which specifies the multi-cell borders.
              crossword.querySelectorAll(".grid-cell__border").forEach((elem) => {
                const style = elem.style["stroke-width"];
                if (style && thickStyle < style) thickStyle = style;
              });

              crossword.querySelectorAll(".grid-cell__border").forEach((elem) => {
                if (elem.style["stroke-width"] === thickStyle) {
                  elem.style["stroke-width"] = "1px";
                }
              });
            }

            bodyPrintArea = document.createElement("div");
            bodyPrintArea.classList.add("body-print-area");
            bodyPrintArea.appendChild(crossword);
            document.body.appendChild(bodyPrintArea);
          }
        } else {
          window.removeEventListener("beforeprint", beforePrint);
        }
      }

      function afterPrint() {
        if (document.contains(cwDiv)) {
          if (document.contains(bodyPrintArea)) {
            document.documentElement.classList.remove(cwStyleClass);
            document.body.removeChild(bodyPrintArea);
          }
        } else {
          window.removeEventListener("afterprint", afterPrint);
        }
      }

      window.addEventListener("beforeprint", beforePrint);
      window.addEventListener("afterprint", afterPrint);
    }
  }, 500);
}

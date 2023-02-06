var wrapper = document.getElementById("keesing-crosswords");

var cwDiv = document.createElement("div");
cwDiv.setAttribute("id", "puzzle-portal");
cwDiv.setAttribute("data-customerid", "ksf");
cwDiv.setAttribute("data-publicpath", "https://web.keesing.com/pub/portal/v2.20.9/dist/");

var cwScript = document.createElement("script");
cwScript.src = "https://web.keesing.com/pub/portal/v2.20.9/dist/main-bundle.js";
cwScript.type = "text/javascript";

wrapper.appendChild(cwDiv);
wrapper.appendChild(cwScript);

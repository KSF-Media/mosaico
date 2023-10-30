const _setTimeout = window.setTimeout;
window.setTimeout = function (fn, timeout, ...args) {
  /* Google Tag Manager waits for 5 seconds before sending data, except for the
   * first data blob which is sent after 500ms. This is unfortunately a bit
   * too slow for us, so we have to do this. Note that this will affect _all_
   * uses of setTimeout!
   */
  if (timeout === 5000) {
    console.log("setTimeout was called with timeout=5000, setting it to 500");
    timeout = 500;
  }
  return _setTimeout(fn, timeout, ...args);
};

import React from "react";
import ReactDOM from "react-dom";
import { BrowserRouter as Router } from "react-router-dom";
import { rehydrateMarks } from "react-imported-component";
window.Buffer = window.Buffer || { isBuffer: () => false };
import "../src/_site.scss";
import "../less/mosaico.less";
import "../affresco/less/Vetrina.less";
import "../affresco/less/Login.less";
import { refreshAdsImpl } from "../src/Mosaico";
import "data-layer-helper/dist/data-layer-helper.js";

// yup, welcome to react 16
import createReactClass from "create-react-class";
React.createClass = createReactClass;
var Mosaico = require("../output/Mosaico/index.js").app();
const loadMosaicoVars = require("../output/Mosaico.Client.Loader/index.js").loadMosaicoVars;
var mosaicoVars;

if (window.mosaicoVars !== undefined) {
  mosaicoVars = Promise.resolve(window.mosaicoVars);
} else {
  mosaicoVars = loadMosaicoVars();
}

function main() {
  Promise.all([mosaicoVars, rehydrateMarks()]).then((res) => {
    const mosaico = (
      <Mosaico
        mosaicoVars={res[0]}
      />
    );
    ReactDOM.hydrate(mosaico, document.getElementById("app"));
  });
}

if (module.hot) {
  module.hot.accept(function () {
    console.log("running main again");
    main();
  });
}

function getGamId(name) {
  let paper = process.env.PAPER;
  // set paper to "test" in .env.local to test ads locally
  return paper + "/" + paper + "_" + name;
}

// Advice from Relevant: Set all the ad slots in viewport on the initial
// page load as non-lazy and all the lower ad slots as lazy.

// When targetId's are changed, remember to change them accordingly in
// Mosaico.purs hooks, Article.purs, classnames in ads.less
window.adSlots = {
  mobile: [
    {
      gamId: getGamId("digiframmob"),
      sizes: [
        [1, 2],
        [300, 100],
        [300, 250],
        [300, 300],
        [300, 431],
        [300, 600],
      ],
      targetId: "mosaico-ad__top-parade",
    },
    {
      gamId: getGamId("mobparad"),
      sizes: [
        [300, 250],
        [300, 300],
        [300, 431],
        [300, 600],
        [320, 320],
      ],
      targetId: "mosaico-ad__mobparad",
    },
    {
      gamId: getGamId("digihelmob"),
      sizes: [300, 431],
      targetId: "mosaico-ad__bigbox1",
    },
    {
      gamId: getGamId("mobmitt"),
      sizes: [
        [300, 100],
        [300, 250],
        [300, 300],
        [300, 341],
        [300, 431],
        [300, 600],
        [320, 320],
      ],
      targetId: "mosaico-ad__bigbox2",
    },
    {
      gamId: getGamId("mobbox1"),
      sizes: [
        [300, 100],
        [300, 250],
        [300, 300],
        [300, 341],
        [300, 431],
        [300, 600],
        [320, 320],
      ],
      targetId: "mosaico-ad__mobbox1",
    },
    {
      gamId: getGamId("mobbox2"),
      sizes: [
        [300, 100],
        [300, 250],
        [300, 300],
        [300, 341],
        [300, 431],
        [300, 600],
      ],
      targetId: "mosaico-ad__mobbox2",
    },
    {
      gamId: getGamId("mobbox3"),
      sizes: [
        [300, 100],
        [300, 250],
        [300, 300],
        [300, 341],
        [300, 431],
        [300, 600],
      ],
      targetId: "mosaico-ad__mobbox3",
    },
  ],
  desktop: [
    {
      gamId: getGamId("digihel"),
      sizes: [
        [620, 891],
        [620, 991],
      ],
      targetId: "mosaico-ad__bigbox1",
    },
    {
      gamId: getGamId("jattebox"),
      sizes: [
        [1, 2],
        [468, 400],
        [468, 600],
      ],
      targetId: "mosaico-ad__bigbox2",
    },
    {
      gamId: getGamId("parad"),
      sizes: [
        [1, 1],
        [1, 2],
        [980, 120],
        [980, 400],
        [980, 552],
        [1600, 1150],
      ],
      targetId: "mosaico-ad__parade",
    },
    {
      gamId: getGamId("maxparad"),
      sizes: [
        [1, 1],
        [1, 2],
        [980, 400],
        [980, 552],
        [980, 600],
        [1920, 1080],
      ],
      targetId: "mosaico-ad__top-parade",
    },
    {
      gamId: getGamId("box1"),
      sizes: [
        [300, 250],
        [300, 300],
        [300, 431],
        [300, 600],
      ],
      targetId: "mosaico-ad__box1",
    },
    {
      gamId: getGamId("box2"),
      sizes: [
        [300, 250],
        [300, 300],
        [300, 431],
        [300, 600],
      ],
      targetId: "mosaico-ad__box2",
    },
    {
      gamId: getGamId("box3"),
      sizes: [
        [300, 250],
        [300, 300],
        [300, 431],
        [300, 600],
      ],
      targetId: "mosaico-ad__box3",
    },
    {
      gamId: getGamId("box4"),
      sizes: [
        [300, 250],
        [300, 300],
        [300, 431],
        [300, 600],
      ],
      targetId: "mosaico-ad__box4",
    },
    {
      gamId: getGamId("box5"),
      sizes: [
        [300, 250],
        [300, 300],
        [300, 431],
        [300, 600],
      ],
      targetId: "mosaico-ad__box5",
    },
    // {
    //   gamId: "wallpaper",
    //   sizes: [ [1600,1200], [1920,1080] ],
    //   targetId: "mosaico-ad__wallpaper",
    // },
  ],
};

window.helper = new DataLayerHelper(window.dataLayer, listener, true);

function listener(_model, message) {
  if (message.event == "cookie_consent_update") {
    // Make sure that googletag.cmd exists.
    window.googletag = window.googletag || {};
    googletag.cmd = googletag.cmd || [];

    if (Cookiebot.consent.marketing) {
      window.userHasSelectedConsent = true;
      window.googletag.cmd.push(function () {
        googletag.pubads().setPrivacySettings({ limitedAds: false });
      });
    } else {
      window.userHasSelectedConsent = false;
      window.googletag.cmd.push(function () {
        googletag.pubads().setPrivacySettings({ limitedAds: true });
      });
    }
    refreshAdsImpl([]);
  }
}

// This listens for DN interactive graphic embeds to find out what height they need to be
// Is there a better location for this code?
window.addEventListener("message", (event) => {
  if (event.data.location && event.data.ratio) {
    const iframeToTarget = document.querySelector(`iframe[src="${event.data.location}"]`)
    iframeToTarget.style.height = iframeToTarget.clientWidth * event.data.ratio + "px";
  }
});

window.googletag = window.googletag || { cmd: [] };
window.googletag.cmd.push(function () {
  // Restrict an ad to just the sites, if key-value targeting is applied
  // to the ad in Google Ad Manager.
  googletag.pubads().setTargeting("sites-or-apps", "sites-only");

  const networkCode = "/21664538223/";

  /* Ad slots to use */
  const slots = window.innerWidth < 1020 ? window.adSlots.mobile : window.adSlots.desktop;
  slots.map((slot) => {
    googletag.defineSlot(networkCode + slot.gamId, slot.sizes, slot.targetId).addService(googletag.pubads());
  });
  window.definedSlots = googletag
    .pubads()
    .getSlots()
    .map((s) => s.getSlotElementId());
  googletag.pubads().collapseEmptyDivs();

  // This prevents ads loading before the user has given or denied consent.
  // If the user made a consent choice earlier, ads are refreshed
  // elsewhere in the code based on that choice.
  googletag.pubads().disableInitialLoad();

  googletag.pubads().enableLazyLoad({
    // Fetch slots within 1 viewport
    fetchMarginPercent: 100,
    // Render slots within 1 viewport
    renderMarginPercent: 100,
    // Double the above values on mobile, where viewports are smaller
    // and users tend to scroll faster
    mobileScaling: 2.0
  });

  googletag.pubads().enableSingleRequest();
  googletag.enableServices();

  googletag.pubads().addEventListener("slotRenderEnded", (event) => {
    if (!event.isEmpty) {
      let elementId = event.slot.getSlotElementId();
      if (elementId !== "mosaico-ad__top-parade") {
        document.querySelector("#" + elementId).classList.add("populated");
      }
    }
  });

  window.addEventListener("message", (event) => {
    let message = event.data;
    if (["BIGMAX", "BIGMOB", "WALLPAPER"].indexOf(message.cmd) != -1) {
      switch (message.cmd) {
        case "BIGMAX":
          var cu = document.getElementById("mosaico-ad__top-parade");
          cu.classList.add("BIGMAX");
          cu.innerHTML = `
            <div>
              <a target="_blank" href="${message.link}">
                <img src="${message.img}">
              </a>
              <div onclick="closeAdDiv('.mosaico-ad.mosaico-ad__top-parade')" class="ad-close">Stäng</div>
            </div>`;

        case "BIGMOB":
          var cu = document.getElementById("mosaico-ad__top-parade");
          cu.classList.add("BIGMOB");
          cu.innerHTML = `
            <div>
              <a target="_blank" href="${message.link}">
                <img src="${message.img}">
              </a>
              <div onclick="closeAdDiv('.mosaico-ad.mosaico-ad__top-parade')" class="ad-close">Stäng</div>
            </div>`;
      }
    }
  });
});

window.closeAdDiv = function (adDiv) {
  document.querySelector(adDiv).innerHTML = "";
};

main();

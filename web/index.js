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

// yup, welcome to react 16
import createReactClass from "create-react-class";
React.createClass = createReactClass;
var Mosaico = require("../output/Mosaico/index.js").jsApp();

function main() {
  rehydrateMarks().then(() => {
    const mosaico = (
      <Mosaico
        article={window.article || null}
        articleType={window.articleType || null}
        mostReadArticles={window.mostReadArticles || null}
        staticPageName={window.staticPageName || null}
        categoryStructure={window.categoryStructure || null}
        globalDisableAds={window.globalDisableAds || null}
        initialFrontpageFeed={window.frontpageFeed || null}
        initialBreakingNews={window.breakingNews || null}
        latestArticles={window.latestArticles || null}
        user={window.user || null}
        entitlements={window.entitlements || null}
        headless={window.headless || null}
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

console.log("starting");

var startConsentCookieSetup = require("../output/Consent.Consent/index.js").startConsentCookieSetupJS();

window.googletag = window.googletag || { cmd: [] };


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
        [300, 100],
        [300, 250],
        [300, 300],
        [300, 431],
        [300, 600],
      ],
      targetId: "mosaico-ad__top-parade",
      isLazy: false,
    },
    {
      gamId: getGamId("mobparad"),
      sizes: [
        [300, 100],
        [300, 250],
        [300, 300],
        [300, 431],
        [300, 600],
      ],
      targetId: "mosaico-ad__mobparad",
      isLazy: false,
    },
    {
      gamId: getGamId("digihelmob"),
      sizes: [300, 431],
      targetId: "mosaico-ad__bigbox1",
      isLazy: true,
    },
    {
      gamId: getGamId("mobmitt"),
      sizes: [
        [300, 100],
        [300, 250],
        [300, 300],
        [300, 431],
        [300, 600],
      ],
      targetId: "mosaico-ad__bigbox2",
      isLazy: true,
    },
    {
      gamId: getGamId("mobbox1"),
      sizes: [
        [300, 100],
        [300, 250],
        [300, 300],
        [300, 431],
        [300, 600],
      ],
      targetId: "mosaico-ad__mobbox1",
      isLazy: true,
    },
    {
      gamId: getGamId("mobbox2"),
      sizes: [
        [300, 100],
        [300, 250],
        [300, 300],
        [300, 431],
        [300, 600],
      ],
      targetId: "mosaico-ad__mobbox2",
      isLazy: true,
    },
    {
      gamId: getGamId("mobbox3"),
      sizes: [
        [300, 100],
        [300, 250],
        [300, 300],
        [300, 431],
        [300, 600],
      ],
      targetId: "mosaico-ad__mobbox3",
      isLazy: true,
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
      isLazy: true,
    },
    {
      gamId: getGamId("jattebox"),
      sizes: [
        [468, 400],
        [468, 600],
      ],
      targetId: "mosaico-ad__bigbox2",
      isLazy: true,
    },
    {
      gamId: getGamId("parad"),
      sizes: [
        [980, 120],
        [980, 400],
        [980, 552],
      ],
      targetId: "mosaico-ad__parade",
      isLazy: false,
    },
    {
      gamId: getGamId("maxparad"),
      sizes: [
        [980, 120],
        [980, 400],
        [980, 480],
        [980, 552],
        [1920, 1080],
      ],
      targetId: "mosaico-ad__top-parade",
      isLazy: false,
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
      isLazy: true,
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
      isLazy: true,
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
      isLazy: true,
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
      isLazy: true,
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
      isLazy: true,
    },
    // {
    //   gamId: "wallpaper",
    //   sizes: [ [1600,1200], [1920,1080] ],
    //   targetId: "mosaico-ad__wallpaper",
    //   isLazy: true
    // },
  ],
};

window.googletag.cmd.push(function () {
  /* Ad slots to use */
  const networkCode = "/21664538223/";

  /* define gam slots */
  const slots = window.innerWidth < 1020 ? window.adSlots.mobile : window.adSlots.desktop;
  slots.map((slot) => {
    googletag.defineSlot(networkCode + slot.gamId, slot.sizes, slot.targetId).addService(googletag.pubads());
  });
  window.definedSlots = googletag
    .pubads()
    .getSlots()
    .map((s) => s.getSlotElementId());
  googletag.pubads().collapseEmptyDivs();

  // This means ads won't load on display, but only on refresh.
  // We've done this so the ads aren't fetched twice, ie once on initial load
  // and then again when we run loadAds to start the programmatic ads auction.
  googletag.pubads().disableInitialLoad();

  googletag.enableServices();
  googletag.pubads().addEventListener("slotRenderEnded", (event) => {
    if (!event.isEmpty) {
      let elementId = event.slot.getSlotElementId();
      document.querySelector("#" + elementId).classList.add("populated");
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

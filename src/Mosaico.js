export function refreshAdsImpl(slotArray) {
  console.log("refreshAdsImpl called");

  if (window.userHasSelectedConsent !== undefined) {
    setTimeout(() => {
      window.loadAds?.();
      console.log("loadAds from Mosaico.js: ", window.loadAds);
    }, 500);
  }
}

export function deleteAdnamiTopscroll() {
  const adnamiTopscroll = document.querySelector(".adnm-html-topscroll-frame-wrapper")
  console.log("deleteAdnamiTopscroll called", adnamiTopscroll);
  if (adnamiTopscroll) {
    adnamiTopscroll.remove();
  }
}

export function setManualScrollRestoration() {
  history.scrollRestoration = "manual";
}

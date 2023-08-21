export function refreshAdsImpl(slotArray) {
  if (window.userHasSelectedConsent !== undefined) {
    setTimeout(() => {
      window.loadAds?.();
      console.log("loadAds from Mosaico.js: ", window.loadAds);
    }, 500);
  }
}

export function setManualScrollRestoration() {
  history.scrollRestoration = "manual";
}

export function getCurrentLocation() {
  return window.location;
}

export function reload() {
  window.location.reload();
}

export function refreshAdsImpl(slotArray) {
  setTimeout(() => {
    window.loadAds?.();
    console.log("loadAds from Mosaico.js: ", window.loadAds);
  }, 500);
}

export function sentryDsn_() {
  return process.env.SENTRY_DSN;
}

export function setManualScrollRestoration() {
  history.scrollRestoration = "manual";
}

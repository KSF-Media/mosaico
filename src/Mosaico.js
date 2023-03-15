export function refreshAdsImpl(slotArray) {
  setTimeout(() => {
    loadAds()
    console.log("loadAds from Mosaico.js: ", loadAds)
  }, 500);
}

export function sentryDsn_() {
  return process.env.SENTRY_DSN;
};

export function setManualScrollRestoration() {
  history.scrollRestoration = 'manual';
}

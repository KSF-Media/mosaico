export function refreshAdsImpl(slotArray) {
  loadAds()
  console.log("loadAds?: ", loadAds)
}

export function sentryDsn_() {
  return process.env.SENTRY_DSN;
};

export function setManualScrollRestoration() {
  history.scrollRestoration = 'manual';
}

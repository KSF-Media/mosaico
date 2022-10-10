export function refreshAdsImpl(slotArray) {
  if (typeof window.googletag.pubads === "function") {
    window.googletag
      .pubads()
      .getSlots()
      .map((s) => {
	if (slotArray.includes(s.getSlotElementId())) {
	  window.googletag.pubads().refresh([s]);
	  // console.log("Refreshing ad: " + s.getSlotElementId());
	}
      });
  }
};
export function sentryDsn_() {
  return process.env.SENTRY_DSN;
};

export function setManualScrollRestoration() {
  history.scrollRestoration = 'manual';
}

export function sendTriggerbeeEvent(email) {
  var mtr_custom = window.mtr_custom || {};
  var mtr = window.mtr || { goal: () => {} };
  mtr_custom.session = { email: email };
  mtr.goal("Logged in");
}

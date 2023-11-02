export function refreshAdsImpl(slotArray) {
  if (window.userHasSelectedConsent !== undefined) {
    const checkIfPubadsReady = setInterval(() => {
      if (window.googletag && googletag.pubadsReady) {
        googletag.pubads().refresh();
        clearInterval(checkIfPubadsReady);
      }
    }, 500);
  }
}

export function deleteAdnamiTopscroll() {
  const adnamiTopscroll = document.querySelector(".adnm-html-topscroll-frame-wrapper");
  console.log("deleteAdnamiTopscroll called", adnamiTopscroll);
  if (adnamiTopscroll) {
    adnamiTopscroll.remove();
  }
}

export function setManualScrollRestoration() {
  history.scrollRestoration = "manual";
}

export function fetchAdImpl(contentUnit) {
  setTimeout(() => {
    try {
      window.googletag.cmd.push(function () {
        if (window.definedSlots.includes(contentUnit)) {
          window.googletag.display(contentUnit);
        }
      });
    } catch (err) {
      console.log(err);
    }
  }, 500);
}

export function getGamId(contentUnit) {
  try {
    const slots = window.innerWidth < 1020 ? window.adSlots.mobile : window.adSlots.desktop;
    const slot = slots.find((element) => contentUnit === element.targetId);
    if (typeof slot === "undefined") {
      return null;
    } else {
      return slot.gamId;
    }
  } catch (err) {
    console.log(err);
    return null;
  }
}

export function showConsentRevocationMessage() {
  if (window.Cookiebot !== undefined) {
    window.Cookiebot.show();
  } else {
    console.error("Cookiebot not loaded!")
  }
}

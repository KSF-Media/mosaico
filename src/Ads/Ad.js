export function fetchAdImpl(contentUnit) {
  setTimeout(() => {
    try {
      window.googletag.cmd.push(function () {
        if (window.definedSlots.includes(contentUnit)) {
          window.googletag
            .pubads()
            .getSlots()
            .map((s) => {
              if (s.getSlotElementId() === contentUnit) {
                window.googletag.pubads().refresh([s]);
              } else {
                window.googletag.display(contentUnit);
              }
            });
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

export function getIsLazy(contentUnit) {
  try {
    const slots = window.innerWidth < 1020 ? window.adSlots.mobile : window.adSlots.desktop;
    const slot = slots.find((element) => contentUnit === element.targetId);
    if (typeof slot === "undefined") {
      return null;
    } else {
      return slot.isLazy;
    }
  } catch (err) {
    console.log(err);
    return null;
  }
}

export function showConsentRevocationMessage() {
  window.googlefc && window.googlefc.showRevocationMessage();
}

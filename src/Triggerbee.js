export function sendTriggerbeeEvent(email) {
  var mtr_custom = window.mtr_custom || {};
  var mtr = window.mtr || { goal: () => {} };
  mtr_custom.session = { email: email };
  mtr.goal("Logged in");
}

export function addToTriggerbeeObj(user) {
    if (!window.triggerbee) {
      window.addEventListener("triggerbeeLoaded", (e) => {
        var triggerbee = window.triggerbee || {};
        triggerbee.user = user || { isLoggedIn: false, isSubscriber: false };
      });
    } else {
      window.triggerbee.user = user;
    }
}

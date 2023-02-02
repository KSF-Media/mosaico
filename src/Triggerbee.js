export function sendTriggerbeeEvent(email) {
  var mtr_custom = window.mtr_custom || {};
  var mtr = window.mtr || { goal: () => {} };
  mtr_custom.session = { email: email };
  mtr.goal("Logged in");
}

export function addToTriggerbeeObj(user) {
  console.log("addToTriggrebeeObj", user);
  if (!window.triggerbee) {
    console.log("Triggerbee not loaded, adding event listener");
    window.addEventListener("triggerbeeLoaded", (e) => {
      console.log("triggerbeeLoaded callback, updating user state");
      var triggerbee = window.triggerbee || {};
      triggerbee.user = user || { isLoggedIn: false, isSubscriber: false };
    });
  } else {
    console.log("Triggerbee loaded, adding directly to triggerbee");
    window.triggerbee.user = user;
    window.triggerbee.widgets.api.init();
  }
}

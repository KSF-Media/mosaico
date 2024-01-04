export function readCookieBotProperties_() {
  return window !== "undefined" && window.Cookiebot;
}

export function showCookieBot(bot) {
  return function() {
    // Needs to be called this way to set the this variable properly
    return bot.show();
  }
}

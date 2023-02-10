/* Light / Dark theme togglers
 *
 * These are in their own file, as we want to run this code before the site
 * renders. Otherwise, there's a good chance that the site will flash in the
 * wrong theme.
 *
 * Running these from index.js is too slow, as the script is loaded with defer.
 */

const autoClass = "theme-hbl-fi-variables";
const darkClass = "theme-hbl-fi-dark-variables";
const lightClass = "theme-hbl-fi-light-variables";

function swapClass(from, to) {
  document.querySelectorAll("." + autoClass).forEach((elem) => {
    elem.classList.remove(from);
    elem.classList.add(to);
  });

  document.documentElement.classList.remove(from);
  document.documentElement.classList.add(to);
}

function setDarkTheme() {
  swapClass(lightClass, darkClass);
}
function setLightTheme() {
  swapClass(darkClass, lightClass);
}

var isDark =
  localStorage.theme === "dark" ||
  (!("theme" in localStorage) && window.matchMedia("(prefers-color-scheme: dark)").matches);
if (isDark) {
  setDarkTheme();
} else {
  setLightTheme();
}

window.toggleDark = () => {
  if (document.documentElement.classList.contains(darkClass)) {
    localStorage.theme = "light";
    setLightTheme();
  } else {
    localStorage.theme = "dark";
    setDarkTheme();
  }
};

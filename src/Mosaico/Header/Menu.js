export function toggleMode() {
  // Defined in index.js
  window.toggleDark();
}

export const startsWith = (needle) => (haystack) => haystack.startsWith(needle);

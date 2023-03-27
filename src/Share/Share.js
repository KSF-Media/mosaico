export function encodeURIComponent_(s) {
  return encodeURIComponent(s);
}

export const nativeShare =
  typeof window !== "undefined" && window.navigator && window.navigator.share
    ? (data) => {
        try {
          window.navigator.share(data);
        } catch (e) {
          /* cancelling share results in an exception */
        }
      }
    : null;

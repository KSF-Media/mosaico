export function encodeURIComponent_(s) {
  return encodeURIComponent(s);
}

export const nativeShare =
  typeof window !== "undefined" && window.navigator && window.navigator.share
    ? async (data) => {
        try {
          await window.navigator.share(data);
        } catch (e) {
        }
      }
    : null;

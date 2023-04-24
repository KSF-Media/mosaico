// Server Port
export const serverPort = process.env.PORT || 8080;

export function log(a) {
  return function () {
    console.log(JSON.stringify(a));
  };
}

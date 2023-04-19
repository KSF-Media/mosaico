// Server Port
export const serverPort = process.env.PORT;
export function log(a) {
  return function () {
    console.log(JSON.stringify(a));
  };
}

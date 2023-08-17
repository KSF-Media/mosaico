// Server Port
export const serverPort = process.env.PORT || 8080;

export function log(a) {
  return function () {
    console.log(JSON.stringify(a));
  };
}

export function logPretty(a) {
  return function () {
    console.log(a);
  };
}

export function logJsEnv(a) {
  return function () {
    const fields = [ 'LETTERA_URL', 'BOTTEGA_URL', 'PERSONA_URL', 'PORT', 'PAPER' ];
    const projection = fields.reduce((acc,k) => {acc[k] = process.env[k]; return acc;}, {});
    console.log({jsEnv: projection})
  }
}

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
    const fields = [ 'LETTERA_URL', 'BOTTEGA_URL', 'PERSONA_URL', 'PORT', 'PAPER', 'HBL_CAMPNO', 'VN_CAMPNO', 'ON_CAMPNO' ];
    const projection = fields.reduce((acc,k) => {acc[k] = process.env[k]; return acc;}, {});
    console.log({jsEnv: projection})
  }
}

export function decodeURIComponent_(uri) {
  try {
    return decodeURIComponent(uri);
  } catch(error) {
    // Just ignore, for our use this is presumed to be impossible
    // since the path has already been processed by Payload.
    return "";
  }
}

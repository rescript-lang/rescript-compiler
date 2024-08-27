

import * as Caml_hash from "./caml_hash.js";

function hash(x) {
  return Caml_hash.hash(10, 100, 0, x);
}

export {
  hash,
}
/* No side effect */

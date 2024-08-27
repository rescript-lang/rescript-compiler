

import * as Caml from "./caml.js";

let compare = Caml.string_compare;

function equal(a, b) {
  return a === b;
}

export {
  compare,
  equal,
}
/* No side effect */



import * as Primitive_string from "./primitive_string.js";

let compare = Primitive_string.compare;

function equal(a, b) {
  return a === b;
}

export {
  compare,
  equal,
}
/* No side effect */

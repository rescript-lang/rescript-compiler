

import * as Primitive_hash from "./Primitive_hash.js";

function hash(x) {
  return Primitive_hash.hash(10, 100, 0, x);
}

export {
  hash,
}
/* No side effect */


import * as Primitive_string from "./primitive_string.js";

function length(prim) {
  return prim.length;
}

function get(prim0, prim1) {
  return prim0.codePointAt(prim1);
}

function unsafe_get(prim0, prim1) {
  return prim0.codePointAt(prim1);
}

let compare = Primitive_string.compare;

function equal(a, b) {
  return a === b;
}

export {
  length,
  get,
  unsafe_get,
  compare,
  equal,
}
/* No side effect */

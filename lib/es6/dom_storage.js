

import * as Primitive_option from "./primitive_option.js";

function getItem(s, obj) {
  return Primitive_option.null_to_opt(obj.getItem(s));
}

function setItem(k, v, obj) {
  obj.setItem(k, v);
}

function removeItem(s, obj) {
  obj.removeItem(s);
}

function key(i, obj) {
  return Primitive_option.null_to_opt(obj.key(i));
}

export {
  getItem,
  setItem,
  removeItem,
  key,
}
/* No side effect */

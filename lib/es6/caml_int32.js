

import * as Caml_js_exceptions from "./caml_js_exceptions.js";

function div(x, y) {
  if (y === 0) {
    throw Caml_js_exceptions.internalMakeExn("Division_by_zero");
  }
  return x / y | 0;
}

function mod_(x, y) {
  if (y === 0) {
    throw Caml_js_exceptions.internalMakeExn("Division_by_zero");
  }
  return x % y;
}

export {
  div,
  mod_,
}
/* No side effect */

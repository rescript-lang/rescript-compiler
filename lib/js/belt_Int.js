'use strict';

var Caml_int32 = require("./caml_int32.js");

function $plus(prim, prim$1) {
  return prim + prim$1 | 0;
}

function $neg(prim, prim$1) {
  return prim - prim$1 | 0;
}

function $slash(prim, prim$1) {
  return prim / prim$1 | 0;
}

var $star = Caml_int32.imul;

exports.$plus = $plus;
exports.$neg = $neg;
exports.$slash = $slash;
exports.$star = $star;
/* No side effect */

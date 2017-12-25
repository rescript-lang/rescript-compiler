'use strict';

var Caml_obj = require("../../lib/js/caml_obj.js");

function f(x, y) {
  return Caml_obj.caml_int_compare(x + y | 0, y + x | 0);
}

function f2(x, y) {
  return Caml_obj.caml_int_compare(x + y | 0, y);
}

var f3 = Caml_obj.caml_int_compare;

function f4(x, y) {
  if (x < y) {
    return x;
  } else {
    return y;
  }
}

exports.f  = f;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
/* No side effect */

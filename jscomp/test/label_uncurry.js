'use strict';

var Caml_format = require("../../lib/js/caml_format.js");

function f(x) {
  return x;
}

function u(x, y) {
  return x + Caml_format.caml_int_of_string(y) | 0;
}

function h(unit) {
  return 3;
}

exports.f = f;
exports.u = u;
exports.h = h;
/* No side effect */

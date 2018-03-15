'use strict';

var Caml_oo_curry = require("../../lib/js/caml_oo_curry.js");

function f(x) {
  x.case = 3;
  return /* () */0;
}

function g(x) {
  return Caml_oo_curry.js2(-977287917, 1, x, 3);
}

exports.f = f;
exports.g = g;
/* No side effect */

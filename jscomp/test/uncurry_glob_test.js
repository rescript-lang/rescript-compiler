'use strict';

var Caml_utils = require("../../lib/js/caml_utils");

var v = Caml_utils.repeat(100, "x");

function M(U) {
  var v = U[/* f */0](100, "x");
  return /* module */[/* v */v];
}

function f() {
  return 3;
}

function $plus$great(a, h) {
  return h(a);
}

function u(h) {
  return h(3);
}

exports.v           = v;
exports.M           = M;
exports.f           = f;
exports.$plus$great = $plus$great;
exports.u           = u;
/* v Not a pure module */

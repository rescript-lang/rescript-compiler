// GENERATED CODE BY BUCKLESCRIPT VERSION 0.7.1 , PLEASE EDIT WITH CARE
'use strict';

var Caml_utils = require("../caml_utils");

var v = Caml_utils.repeat(100, "x");

function M(U) {
  var v = U[/* f */0](100, "x");
  return /* module */[/* v */v];
}

function f() {
  return 3;
}

f();

function $plus$great(a, h) {
  return h(a);
}

function u(h) {
  return $plus$great(3, h);
}

exports.v           = v;
exports.M           = M;
exports.f           = f;
exports.$plus$great = $plus$great;
exports.u           = u;
/* v Not a pure module */

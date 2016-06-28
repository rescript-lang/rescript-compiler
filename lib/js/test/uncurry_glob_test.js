// GENERATED CODE BY BUCKLESCRIPT VERSION 0.6.2 , PLEASE EDIT WITH CARE
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

var u = f();

exports.v = v;
exports.M = M;
exports.f = f;
exports.u = u;
/* v Not a pure module */

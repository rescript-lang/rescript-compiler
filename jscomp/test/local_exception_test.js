'use strict';

var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var A = Caml_exceptions.create("Local_exception_test.A");

var v = {
  CamlExt: A,
  _1: 3,
  _2: true
};

var B = Caml_exceptions.create("Local_exception_test.B");

var u = {
  CamlExt: B
};

var D = Caml_exceptions.create("Local_exception_test.D");

var d = {
  CamlExt: D,
  _1: 3
};

exports.A = A;
exports.v = v;
exports.B = B;
exports.u = u;
exports.D = D;
exports.d = d;
/* No side effect */

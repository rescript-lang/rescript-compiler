'use strict';

var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var A = Caml_exceptions.create("Local_exception_test.A");

var v = {
  RE_EXN_ID: A,
  _1: 3,
  _2: true
};

var B = Caml_exceptions.create("Local_exception_test.B");

var u = {
  RE_EXN_ID: B
};

var D = Caml_exceptions.create("Local_exception_test.D");

var d = {
  RE_EXN_ID: D,
  _1: 3
};

exports.A = A;
exports.v = v;
exports.B = B;
exports.u = u;
exports.D = D;
exports.d = d;
/* No side effect */

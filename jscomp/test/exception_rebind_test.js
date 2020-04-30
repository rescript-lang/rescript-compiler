'use strict';

var Exception_def = require("./exception_def.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var E = Caml_exceptions.create("Exception_rebind_test.A.E");

var A = {
  E: E
};

var B = {
  F: E
};

var A0 = Caml_exceptions.create("Exception_rebind_test.A0");

var H0 = "Invalid_argument";

var u0 = {
  ExceptionID: H0,
  _1: "x"
};

var u1 = {
  ExceptionID: "Invalid_argument",
  _1: "x"
};

var u2 = {
  ExceptionID: "Not_found"
};

var H = Exception_def.A;

var H1 = H0;

exports.A = A;
exports.B = B;
exports.H = H;
exports.A0 = A0;
exports.H0 = H0;
exports.H1 = H1;
exports.u0 = u0;
exports.u1 = u1;
exports.u2 = u2;
/* Exception_def Not a pure module */

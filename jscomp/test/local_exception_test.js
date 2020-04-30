'use strict';

var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var A = Caml_exceptions.create("Local_exception_test.A");

var v_000 = A.ExceptionID;

var v_003 = A.Debug;

var v = {
  ExceptionID: v_000,
  _1: 3,
  _2: true,
  Debug: v_003
};

var B = Caml_exceptions.create("Local_exception_test.B");

var u_000 = B.ExceptionID;

var u_001 = B.Debug;

var u = {
  ExceptionID: u_000,
  Debug: u_001
};

var D = Caml_exceptions.create("Local_exception_test.D");

var d_000 = D.ExceptionID;

var d_002 = D.Debug;

var d = {
  ExceptionID: d_000,
  _1: 3,
  Debug: d_002
};

exports.A = A;
exports.v = v;
exports.B = B;
exports.u = u;
exports.D = D;
exports.d = d;
/* No side effect */

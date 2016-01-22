// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

var Caml_exceptions = require("../runtime/caml_exceptions");
var Test_common     = require("./test_common");

var Local = [
  248,
  "Test_exception.Local",
  ++ Caml_exceptions.caml_oo_last_id
];

function f() {
  throw [
        0,
        Local,
        3
      ];
}

function g() {
  throw Caml_exceptions.Not_found;
}

function h() {
  throw [
        0,
        Test_common.U,
        3
      ];
}

function x() {
  throw Test_common.H;
}

function xx() {
  throw [
        0,
        Caml_exceptions.Invalid_argument,
        "x"
      ];
}

exports.Local = Local;
exports.f     = f;
exports.g     = g;
exports.h     = h;
exports.x     = x;
exports.xx    = xx;
/* No side effect */

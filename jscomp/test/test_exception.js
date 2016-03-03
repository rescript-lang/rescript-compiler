// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Test_common             = require("./test_common");

var Local = {
  0: "Test_exception.Local",
  1: Caml_builtin_exceptions.get_id(),
  length: 2,
  tag: 248
};

function f() {
  throw [
        Local,
        3
      ];
}

function g() {
  throw Caml_builtin_exceptions.not_found;
}

function h() {
  throw [
        Test_common.U,
        3
      ];
}

function x() {
  throw Test_common.H;
}

function xx() {
  throw [
        Caml_builtin_exceptions.invalid_argument,
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

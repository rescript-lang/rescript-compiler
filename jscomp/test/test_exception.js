'use strict';

var Test_common = require("./test_common.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var Local = Caml_exceptions.create("Test_exception.Local");

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

var Nullary = Caml_exceptions.create("Test_exception.Nullary");

var a = Nullary;

exports.Local = Local;
exports.f = f;
exports.g = g;
exports.h = h;
exports.x = x;
exports.xx = xx;
exports.Nullary = Nullary;
exports.a = a;
/* No side effect */

'use strict';

var Test_common = require("./test_common.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var Local = Caml_exceptions.create("Test_exception.Local");

function f(param) {
  throw {
        RE_EXN_ID: Local,
        _1: 3
      };
}

function g(param) {
  throw {
        RE_EXN_ID: "Not_found"
      };
}

function h(param) {
  throw {
        RE_EXN_ID: Test_common.U,
        _1: 3
      };
}

function x(param) {
  throw {
        RE_EXN_ID: Test_common.H
      };
}

function xx(param) {
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "x"
      };
}

var Nullary = Caml_exceptions.create("Test_exception.Nullary");

var a = {
  RE_EXN_ID: Nullary
};

exports.Local = Local;
exports.f = f;
exports.g = g;
exports.h = h;
exports.x = x;
exports.xx = xx;
exports.Nullary = Nullary;
exports.a = a;
/* No side effect */

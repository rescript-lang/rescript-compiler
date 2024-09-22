// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Test_common = require("./test_common.js");
let Primitive_exceptions = require("../../lib/js/primitive_exceptions.js");

let Local = /* @__PURE__ */Primitive_exceptions.create("Test_exception.Local");

function f() {
  throw {
    RE_EXN_ID: Local,
    _1: 3,
    Error: new Error()
  };
}

function g() {
  throw {
    RE_EXN_ID: "Not_found",
    Error: new Error()
  };
}

function h() {
  throw {
    RE_EXN_ID: Test_common.U,
    _1: 3,
    Error: new Error()
  };
}

function x() {
  throw {
    RE_EXN_ID: Test_common.H,
    Error: new Error()
  };
}

function xx() {
  throw {
    RE_EXN_ID: "Invalid_argument",
    _1: "x",
    Error: new Error()
  };
}

let Nullary = /* @__PURE__ */Primitive_exceptions.create("Test_exception.Nullary");

let a = {
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

// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_exceptions = require("../runtime/caml_exceptions");

function f(param) {
  switch (param) {
    case "aaaabb" : 
        return 0;
    case "bbbb" : 
        return 1;
    default:
      throw [
            0,
            Caml_exceptions.Assert_failure,
            [
              0,
              "test_string.ml",
              4,
              18
            ]
          ];
  }
}

function a(x) {
  return "helloworldhello" + x;
}

function b(y, x) {
  return y + ("helloworldhello" + x);
}

function c(x, y) {
  return x + "hellohiuhi" + y;
}

var v = 2;

function h(s, b) {
  return +(s[0] === "a" && b[0] === /* "b" */98 && s.charCodeAt(1) === b[2]);
}

exports.f = f;
exports.a = a;
exports.b = b;
exports.c = c;
exports.v = v;
exports.h = h;
/* No side effect */

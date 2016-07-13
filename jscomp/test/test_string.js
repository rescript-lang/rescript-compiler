'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions");

function f(param) {
  switch (param) {
    case "aaaabb" : 
        return 0;
    case "bbbb" : 
        return 1;
    default:
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
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

function h(s, b) {
  if (s[0] === "a" && b[0] === /* "b" */98) {
    return +(s.charCodeAt(1) === b[2]);
  }
  else {
    return /* false */0;
  }
}

var v = 2;

exports.f = f;
exports.a = a;
exports.b = b;
exports.c = c;
exports.v = v;
exports.h = h;
/* No side effect */

// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


function f(x) {
  switch (x) {
    case "aaaabb" :
      return 0;
    case "bbbb" :
      return 1;
    default:
      throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "test_string.res",
          5,
          17
        ],
        Error: new Error()
      };
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

let v = "xx".length;

function h(s) {
  return s.codePointAt(0) === /* 'a' */97;
}

exports.f = f;
exports.a = a;
exports.b = b;
exports.c = c;
exports.v = v;
exports.h = h;
/* No side effect */

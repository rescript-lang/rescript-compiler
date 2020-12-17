'use strict';


function test(code) {
  if (code > 599 || code < 500) {
    if (code === 201 || code === 200) {
      return "good response";
    } else {
      return "the catch all";
    }
  } else if (code > 597 || code < 512) {
    return "bad response";
  } else {
    return "the catch all";
  }
}

var a = "good response";

var b = "bad response";

if (a !== "good response") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "gpr_3877_test.ml",
          14,
          3
        ],
        Error: new Error()
      };
}

if (b !== "bad response") {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "gpr_3877_test.ml",
          15,
          3
        ],
        Error: new Error()
      };
}

exports.test = test;
exports.a = a;
exports.b = b;
/*  Not a pure module */

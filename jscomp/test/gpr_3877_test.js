// Generated by ReScript, PLEASE EDIT WITH CARE
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

let a = "good response";

let b = "bad response";

if (a !== "good response") {
  throw new Error("Assert_failure", {
        cause: {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "gpr_3877_test.res",
            26,
            0
          ]
        }
      });
}

if (b !== "bad response") {
  throw new Error("Assert_failure", {
        cause: {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "gpr_3877_test.res",
            27,
            0
          ]
        }
      });
}

exports.test = test;
exports.a = a;
exports.b = b;
/*  Not a pure module */

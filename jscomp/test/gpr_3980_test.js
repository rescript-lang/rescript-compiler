'use strict';

var Js_math = require("../../lib/js/js_math.js");

var match = 1;

if (match !== undefined) {
  if (match !== 1) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "gpr_3980_test.res",
            15,
            7
          ],
          Error: new Error()
        };
  }
  var match$1 = 1;
  if (match$1 !== 1) {
    if (match$1 !== 2) {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "gpr_3980_test.res",
              13,
              9
            ],
            Error: new Error()
          };
    }
    Js_math.floor(1);
  }
  
} else {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "gpr_3980_test.res",
          15,
          7
        ],
        Error: new Error()
      };
}

/*  Not a pure module */

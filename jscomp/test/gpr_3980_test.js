'use strict';

var Js_math = require("../../lib/js/js_math.js");

var match = 1;

if (match !== undefined) {
  if (match !== 1) {
    throw {
          ExceptionID: "Assert_failure",
          _1: /* tuple */[
            "gpr_3980_test.ml",
            16,
            10
          ]
        };
  }
  var match$1 = 1;
  if (match$1 !== 1) {
    if (match$1 !== 2) {
      throw {
            ExceptionID: "Assert_failure",
            _1: /* tuple */[
              "gpr_3980_test.ml",
              14,
              12
            ]
          };
    }
    ({
        name: "bye",
        age: Js_math.floor(1)
      });
  }
  
} else {
  throw {
        ExceptionID: "Assert_failure",
        _1: /* tuple */[
          "gpr_3980_test.ml",
          16,
          10
        ]
      };
}

/*  Not a pure module */

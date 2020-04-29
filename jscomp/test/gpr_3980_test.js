'use strict';

var Js_math = require("../../lib/js/js_math.js");

var match = 1;

if (match !== undefined) {
  if (match !== 1) {
    throw {
          ExceptionID: -9,
          _1: /* tuple */[
            "gpr_3980_test.ml",
            16,
            10
          ],
          Debug: "Assert_failure"
        };
  }
  var match$1 = 1;
  if (match$1 !== 1) {
    if (match$1 !== 2) {
      throw {
            ExceptionID: -9,
            _1: /* tuple */[
              "gpr_3980_test.ml",
              14,
              12
            ],
            Debug: "Assert_failure"
          };
    }
    ({
        name: "bye",
        age: Js_math.floor(1)
      });
  }
  
} else {
  throw {
        ExceptionID: -9,
        _1: /* tuple */[
          "gpr_3980_test.ml",
          16,
          10
        ],
        Debug: "Assert_failure"
      };
}

/*  Not a pure module */

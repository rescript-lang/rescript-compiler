'use strict';

var Js_math = require("../../lib/js/js_math.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var match = 1;

if (match === undefined) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "gpr_3980_test.ml",
          16,
          10
        ]
      ];
}

if (match !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "gpr_3980_test.ml",
          16,
          10
        ]
      ];
}

var match$1 = 1;

if (match$1 !== 1) {
  if (match$1 !== 2) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "gpr_3980_test.ml",
            14,
            12
          ]
        ];
  }
  ({
      name: "bye",
      age: Js_math.floor(1)
    });
}

/*  Not a pure module */

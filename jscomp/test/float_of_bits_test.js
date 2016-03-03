// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_float = require("../runtime/caml_float");
var Mt         = require("./mt");

var one_float = /* int64 */[
  0,
  1072693248
];

var suites_000 = /* tuple */[
  "one",
  function () {
    return /* Eq */{
            0: Caml_float.caml_int64_bits_of_float(1.0),
            1: one_float,
            length: 2,
            tag: 0
          };
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "two",
    function () {
      return /* Eq */{
              0: Caml_float.caml_int64_float_of_bits(one_float),
              1: 1.0,
              length: 2,
              tag: 0
            };
    }
  ],
  /* [] */0
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("float_of_bits_test.ml", suites);

exports.one_float = one_float;
exports.suites    = suites;
/*  Not a pure module */

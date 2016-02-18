// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Mt                      = require("./mt");

var Str = {
  0: "Extensible_variant_test.Str",
  1: ++ Caml_builtin_exceptions.caml_oo_last_id,
  length: 2,
  tag: 248
};

var Int = {
  0: "Extensible_variant_test.N.Int",
  1: ++ Caml_builtin_exceptions.caml_oo_last_id,
  length: 2,
  tag: 248
};

var N = [Int];

var Int$1 = {
  0: "Extensible_variant_test.Int",
  1: ++ Caml_builtin_exceptions.caml_oo_last_id,
  length: 2,
  tag: 248
};

function to_int(x) {
  if (x[0] === Str) {
    return -1;
  }
  else if (x[0] === Int) {
    return x[1];
  }
  else if (x[0] === Int$1) {
    return x[2];
  }
  else {
    throw [
          Caml_builtin_exceptions.Assert_failure,
          [
            "extensible_variant_test.ml",
            16,
            9
          ]
        ];
  }
}

var suites_000 = /* tuple */[
  "test_int",
  function () {
    return /* Eq */{
            0: 3,
            1: to_int([
                  Int,
                  3,
                  0
                ]),
            length: 2,
            tag: 0
          };
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "test_int2",
    function () {
      return /* Eq */{
              0: 0,
              1: to_int([
                    Int$1,
                    3,
                    0
                  ]),
              length: 2,
              tag: 0
            };
    }
  ],
  /* :: */[
    /* tuple */[
      "test_string",
      function () {
        return /* Eq */{
                0: -1,
                1: to_int([
                      Str,
                      "x"
                    ]),
                length: 2,
                tag: 0
              };
      }
    ],
    /* [] */0
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("extensible_variant_test.ml", suites);

exports.Str    = Str;
exports.N      = N;
exports.Int    = Int$1;
exports.to_int = to_int;
exports.suites = suites;
/*  Not a pure module */

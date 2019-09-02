'use strict';

var Mt = require("./mt.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var Str = Caml_exceptions.create("Extensible_variant_test.Str");

var Int = Caml_exceptions.create("Extensible_variant_test.N.Int");

var N = {
  Int: Int
};

var Int$1 = Caml_exceptions.create("Extensible_variant_test.Int");

function to_int(x) {
  if (x[0] === Str) {
    return -1;
  } else if (x[0] === Int) {
    return x[1];
  } else if (x[0] === Int$1) {
    return x[2];
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "extensible_variant_test.ml",
            16,
            9
          ]
        ];
  }
}

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "test_int",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: 3,
                Arg1: to_int([
                      Int,
                      3,
                      0
                    ])
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "test_int2",
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: 0,
                  Arg1: to_int([
                        Int$1,
                        3,
                        0
                      ])
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "test_string",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: -1,
                    Arg1: to_int([
                          Str,
                          "x"
                        ])
                  };
          })
      ],
      Arg1: "[]"
    }
  }
};

Mt.from_pair_suites("Extensible_variant_test", suites);

exports.Str = Str;
exports.N = N;
exports.Int = Int$1;
exports.to_int = to_int;
exports.suites = suites;
/*  Not a pure module */

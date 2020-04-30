'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var Str = Caml_exceptions.create("Extensible_variant_test.Str");

var Int = Caml_exceptions.create("Extensible_variant_test.N.Int");

var N = {
  Int: Int
};

var Int$1 = Caml_exceptions.create("Extensible_variant_test.Int");

function to_int(x) {
  if (x.RE_EXN_ID === Str) {
    return -1;
  }
  if (x.RE_EXN_ID === Int) {
    return x._1;
  }
  if (x.RE_EXN_ID === Int$1) {
    return x._2;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: /* tuple */[
          "extensible_variant_test.ml",
          16,
          9
        ]
      };
}

var suites_000 = /* tuple */[
  "test_int",
  (function (param) {
      return /* Eq */Block.__(0, [
                3,
                to_int({
                      RE_EXN_ID: Int,
                      _1: 3,
                      _2: 0
                    })
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "test_int2",
    (function (param) {
        return /* Eq */Block.__(0, [
                  0,
                  to_int({
                        RE_EXN_ID: Int$1,
                        _1: 3,
                        _2: 0
                      })
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "test_string",
      (function (param) {
          return /* Eq */Block.__(0, [
                    -1,
                    to_int({
                          RE_EXN_ID: Str,
                          _1: "x"
                        })
                  ]);
        })
    ],
    /* [] */0
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("Extensible_variant_test", suites);

exports.Str = Str;
exports.N = N;
exports.Int = Int$1;
exports.to_int = to_int;
exports.suites = suites;
/*  Not a pure module */

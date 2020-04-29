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
  if (x.ExceptionID === Str.ExceptionID) {
    return -1;
  }
  if (x.ExceptionID === Int.ExceptionID) {
    return x._1;
  }
  if (x.ExceptionID === Int$1.ExceptionID) {
    return x._2;
  }
  throw {
        ExceptionID: -9,
        _1: /* tuple */[
          "extensible_variant_test.ml",
          16,
          9
        ],
        Debug: "Assert_failure"
      };
}

var suites_000 = /* tuple */[
  "test_int",
  (function (param) {
      return /* Eq */Block.__(0, [
                3,
                to_int({
                      ExceptionID: Int.ExceptionID,
                      _1: 3,
                      _2: 0,
                      Debug: Int.Debug
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
                        ExceptionID: Int$1.ExceptionID,
                        _1: 3,
                        _2: 0,
                        Debug: Int$1.Debug
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
                          ExceptionID: Str.ExceptionID,
                          _1: "x",
                          Debug: Str.Debug
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

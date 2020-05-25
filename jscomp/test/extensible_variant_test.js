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
        ],
        Error: new Error()
      };
}

var suites_000 = /* tuple */[
  "test_int",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: 3,
              _1: to_int({
                    RE_EXN_ID: Int,
                    _1: 3,
                    _2: 0
                  })
            };
    })
];

var suites_001 = /* :: */{
  _0: /* tuple */[
    "test_int2",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: 0,
                _1: to_int({
                      RE_EXN_ID: Int$1,
                      _1: 3,
                      _2: 0
                    })
              };
      })
  ],
  _1: /* :: */{
    _0: /* tuple */[
      "test_string",
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: -1,
                  _1: to_int({
                        RE_EXN_ID: Str,
                        _1: "x"
                      })
                };
        })
    ],
    _1: /* [] */0
  }
};

var suites = /* :: */{
  _0: suites_000,
  _1: suites_001
};

Mt.from_pair_suites("Extensible_variant_test", suites);

exports.Str = Str;
exports.N = N;
exports.Int = Int$1;
exports.to_int = to_int;
exports.suites = suites;
/*  Not a pure module */

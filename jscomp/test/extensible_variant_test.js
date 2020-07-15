'use strict';

var Mt = require("./mt.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var Str = Caml_exceptions.create("Extensible_variant_test.Str");

var Int = Caml_exceptions.create("Extensible_variant_test.N.Int");

var N = {
  Int
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
        _1: [
          "extensible_variant_test.ml",
          16,
          9
        ],
        Error: new Error()
      };
}

var suites_0 = [
  "test_int",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: 3,
              _1: to_int({
                    RE_EXN_ID: Int,
                    _1: 3,
                    _2: 0
                  })
            };
    })
];

var suites_1 = {
  hd: [
    "test_int2",
    (function (param) {
        return {
                TAG: /* Eq */0,
                _0: 0,
                _1: to_int({
                      RE_EXN_ID: Int$1,
                      _1: 3,
                      _2: 0
                    })
              };
      })
  ],
  tl: {
    hd: [
      "test_string",
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: -1,
                  _1: to_int({
                        RE_EXN_ID: Str,
                        _1: "x"
                      })
                };
        })
    ],
    tl: /* [] */0
  }
};

var suites = {
  hd: suites_0,
  tl: suites_1
};

Mt.from_pair_suites("Extensible_variant_test", suites);

exports.Str = Str;
exports.N = N;
exports.Int = Int$1;
exports.to_int = to_int;
exports.suites = suites;
/*  Not a pure module */

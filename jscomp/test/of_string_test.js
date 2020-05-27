'use strict';

var Mt = require("./mt.js");
var Pervasives = require("../../lib/js/pervasives.js");

var suites_0 = [
  "string_of_float_1",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: "10.",
              _1: Pervasives.string_of_float(10)
            };
    })
];

var suites_1 = {
  hd: [
    "string_of_int",
    (function (param) {
        return {
                TAG: /* Eq */0,
                _0: "10",
                _1: String(10)
              };
      })
  ],
  tl: {
    hd: [
      "valid_float_lexem",
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: "10.",
                  _1: Pervasives.valid_float_lexem("10")
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

Mt.from_pair_suites("Of_string_test", suites);

exports.suites = suites;
/*  Not a pure module */

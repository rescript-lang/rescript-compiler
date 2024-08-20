// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Pervasives = require("../../lib/js/pervasives.js");

let suites_0 = [
  "string_of_float_1",
  (param => ({
    TAG: "Eq",
    _0: "10.",
    _1: Pervasives.string_of_float(10)
  }))
];

let suites_1 = {
  hd: [
    "string_of_int",
    (param => ({
      TAG: "Eq",
      _0: "10",
      _1: String(10)
    }))
  ],
  tl: {
    hd: [
      "valid_float_lexem",
      (param => ({
        TAG: "Eq",
        _0: "10.",
        _1: Pervasives.valid_float_lexem("10")
      }))
    ],
    tl: /* [] */0
  }
};

let suites = {
  hd: suites_0,
  tl: suites_1
};

Mt.from_pair_suites("Of_string_test", suites);

exports.suites = suites;
/*  Not a pure module */

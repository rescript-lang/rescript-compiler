'use strict';

var Mt         = require("./mt");
var Block      = require("../../lib/js/block");
var Pervasives = require("../../lib/js/pervasives");

var suites_000 = /* tuple */[
  "string_of_float_1",function () {
    return /* Eq */Block.__(0, [
              "10.",Pervasives.string_of_float(10)
            ]);
  }
];

var suites_001 = /* Nested :: */[
  /* tuple */[
    "string_of_int",function () {
      return /* Eq */Block.__(0, [
                "10","" + 10
              ]);
    }
  ],[
    /* tuple */[
      "valid_float_lexem",function () {
        return /* Eq */Block.__(0, [
                  "10.",Pervasives.valid_float_lexem("10")
                ]);
      }
    ],/* [] */0
  ]
];

var suites = /* Nested :: */[
  suites_000,suites_001
];

Mt.from_pair_suites("of_string_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */

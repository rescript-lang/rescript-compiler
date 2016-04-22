// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt    = require("./mt");
var Block = require("../runtime/block");

var suites_000 = /* tuple */[
  "int_type",
  function () {
    return /* Eq */Block.__(0, [
              "number",
              "number"
            ]);
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "string_type",
    function () {
      return /* Eq */Block.__(0, [
                "string",
                "string"
              ]);
    }
  ],
  /* [] */0
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("typeof_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */

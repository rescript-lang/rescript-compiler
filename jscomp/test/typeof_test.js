// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

var Mt = require("./mt");

var suites_001 = [
  /* tuple */0,
  "int_type",
  function () {
    return [
            /* Eq */0,
            "number",
            "number"
          ];
  }
];

var suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "string_type",
    function () {
      return [
              /* Eq */0,
              "string",
              "string"
            ];
    }
  ],
  /* [] */0
];

var suites = [
  /* :: */0,
  suites_001,
  suites_002
];

Mt.from_pair_suites("typeof_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */

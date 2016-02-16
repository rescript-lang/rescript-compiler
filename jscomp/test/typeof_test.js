// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt = require("./mt");

var suites_000 = /* tuple */[
  "int_type",
  function () {
    return /* Eq */{
            0: "number",
            1: "number",
            length: 2,
            tag: 0
          };
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "string_type",
    function () {
      return /* Eq */{
              0: "string",
              1: "string",
              length: 2,
              tag: 0
            };
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

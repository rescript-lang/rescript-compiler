// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Pervasives = require("../stdlib/pervasives");
var Mt         = require("./mt");

var suites_000 = /* tuple */[
  "string_of_float_1",
  function () {
    return /* Eq */{
            0: "10.",
            1: Pervasives.string_of_float(10),
            length: 2,
            tag: 0
          };
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "string_of_int",
    function () {
      return /* Eq */{
              0: "10",
              1: "" + 10,
              length: 2,
              tag: 0
            };
    }
  ],
  /* :: */[
    /* tuple */[
      "valid_float_lexem",
      function () {
        return /* Eq */{
                0: "10.",
                1: Pervasives.valid_float_lexem("10"),
                length: 2,
                tag: 0
              };
      }
    ],
    /* [] */0
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("of_string_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */

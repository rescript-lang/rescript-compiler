// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Pervasives = require("../stdlib/pervasives");
var Mt = require("./mt");

var suites_001 = [
  /* tuple */0,
  "string_of_float_1",
  function () {
    return [
            /* Eq */0,
            "10.",
            Pervasives.string_of_float(10)
          ];
  }
];

var suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "string_of_int",
    function () {
      return [
              /* Eq */0,
              "10",
              Pervasives.string_of_int(10)
            ];
    }
  ],
  [
    /* :: */0,
    [
      /* tuple */0,
      "valid_float_lexem",
      function () {
        return [
                /* Eq */0,
                "10.",
                Pervasives.valid_float_lexem("10")
              ];
      }
    ],
    /* [] */0
  ]
];

var suites = [
  /* :: */0,
  suites_001,
  suites_002
];

Mt.from_pair_suites("of_string_test.ml", suites);

exports.suites = suites;
/*  fail the pure module */

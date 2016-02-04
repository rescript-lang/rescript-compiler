// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt = require("./mt");

console.log(JSON.stringify([
          /* :: */0,
          1,
          [
            /* :: */0,
            2,
            [
              /* :: */0,
              3,
              /* [] */0
            ]
          ]
        ]));

console.log("hey");

var suites_001 = [
  /* tuple */0,
  "anything_to_string",
  function () {
    return [
            /* Eq */0,
            "3",
            "" + 3
          ];
  }
];

var suites = [
  /* :: */0,
  suites_001,
  /* [] */0
];

Mt.from_pair_suites("lib_js_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */

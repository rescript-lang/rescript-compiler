// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt = require("./mt");

console.log(JSON.stringify(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ]));

console.log("hey");

var suites_000 = /* tuple */[
  "anything_to_string",
  function () {
    return /* Eq */{
            0: "3",
            1: "" + 3,
            length: 2,
            tag: 0
          };
  }
];

var suites = /* :: */[
  suites_000,
  /* [] */0
];

Mt.from_pair_suites("lib_js_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */

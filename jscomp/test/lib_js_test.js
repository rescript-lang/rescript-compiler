'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

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
  (function () {
      return /* Eq */Block.__(0, [
                "3",
                String(3)
              ]);
    })
];

var suites = /* :: */[
  suites_000,
  /* [] */0
];

Mt.from_pair_suites("lib_js_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */

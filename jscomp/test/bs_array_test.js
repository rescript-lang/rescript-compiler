'use strict';

var Mt       = require("./mt.js");
var Block    = require("../../lib/js/block.js");
var Bs_array = require("../../lib/js/bs_array.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      function () {
        return /* Eq */Block.__(0, [
                  x,
                  y
                ]);
      }
    ],
    suites[0]
  ];
  return /* () */0;
}

console.log(/* int array */[
            1,
            2,
            3,
            4
          ].filter(function (x) {
              return +(x > 2);
            }).map(function (x, i) {
            return x + i | 0;
          }).reduce(function (x, y) {
          return x + y | 0;
        }, 0));

eq("File \"bs_array_test.ml\", line 22, characters 5-12", Bs_array.ofList(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ]), /* int array */[
      1,
      2,
      3
    ]);

eq("File \"bs_array_test.ml\", line 23, characters 6-13", Bs_array.map(function (x) {
          return x + 1 | 0;
        }, /* int array */[
          1,
          2,
          3
        ]), /* int array */[
      2,
      3,
      4
    ]);

Mt.from_pair_suites("File \"bs_array_test.ml\", line 27, characters 23-30", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
/*  Not a pure module */

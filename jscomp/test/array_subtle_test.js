'use strict';

var Block = require("../../lib/js/block");
var Mt    = require("./mt");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
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

var v = /* int array */[
  1,
  2,
  3,
  3
];

eq('File "array_subtle_test.ml", line 12, characters 5-12', /* tuple */[
      4,
      v.length
    ]);

eq('File "array_subtle_test.ml", line 15, characters 5-12', /* tuple */[
      5,
      v.push(3)
    ]);

eq('File "array_subtle_test.ml", line 16, characters 5-12', /* tuple */[
      5,
      v.length
    ]);

eq('File "array_subtle_test.ml", line 17, characters 5-12', /* tuple */[
      5,
      v.length
    ]);

eq('File "array_subtle_test.ml", line 21, characters 5-12', /* tuple */[
      3,
      v[2]
    ]);

v[2] = 4;

eq('File "array_subtle_test.ml", line 23, characters 5-12', /* tuple */[
      4,
      v[2]
    ]);

while(v.length > 0) {
  v.pop();
};

eq('File "array_subtle_test.ml", line 29, characters 5-12', /* tuple */[
      0,
      v.length
    ]);

Mt.from_pair_suites("array_subtle_test.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
exports.v       = v;
/*  Not a pure module */

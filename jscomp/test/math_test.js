'use strict';

var Js_math = require("../../lib/js/js_math");
var Mt      = require("./mt");
var Block   = require("../../lib/js/block");

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

eq('File "math_test.ml", line 12, characters 7-14', Js_math.ceil(3.2), 4);

eq('File "math_test.ml", line 13, characters 7-14', Js_math.floor(3.2), 3);

var a = Js_math.random_int(1, 3);

eq('File "math_test.ml", line 14, characters 7-14', /* true */1, +(a >= 1 && a < 3));

Mt.from_pair_suites("math_test.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
/*  Not a pure module */

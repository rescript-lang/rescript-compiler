'use strict';

var Mt    = require("./mt");
var Block = require("../../lib/js/block");

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

var v = [0];

var h = {
  hi: 2,
  lo: 0
};

eq('File "bs_ignore_effect.ml", line 16, characters 5-12', v[0], 1);

Mt.from_pair_suites("bs_ignore_effect.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
exports.v       = v;
exports.h       = h;
/* h Not a pure module */

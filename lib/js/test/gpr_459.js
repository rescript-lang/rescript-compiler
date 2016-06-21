// GENERATED CODE BY BUCKLESCRIPT VERSION 0.5.5 , PLEASE EDIT WITH CARE
'use strict';

var Mt    = require("./mt");
var Block = require("../block");

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

var uu = {
  "'x": 3
};

var hh = uu["'x"];

eq('File "gpr_459.ml", line 17, characters 12-19', hh, 3);

Mt.from_pair_suites("gpr_459.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
exports.uu      = uu;
exports.hh      = hh;
/* uu Not a pure module */

'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}


function add(x,y){
  return x + y
}

;

var v = [0];

var h = (v[0] = v[0] + 1 | 0, {
    hi: 2,
    lo: 0
  });

var z = (v[0] = v[0] + 1 | 0, add(3.0, 2.0));

eq("File \"bs_ignore_effect.ml\", line 26, characters 5-12", v[0], 2);

eq("File \"bs_ignore_effect.ml\", line 27, characters 5-12", z, 5.0);

Mt.from_pair_suites("bs_ignore_effect.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.v = v;
exports.h = h;
exports.z = z;
/*  Not a pure module */

// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Mt = require("./mt.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

var others = [
  0,
  0,
  1,
  1,
  2e3
];

eq("File \"gpr_3549_test.res\", line 13, characters 5-12", 7.0, 7);

eq("File \"gpr_3549_test.res\", line 14, characters 5-12", 2e3, 2000);

eq("File \"gpr_3549_test.res\", line 15, characters 5-12", 0.2, 0.2);

eq("File \"gpr_3549_test.res\", line 16, characters 5-12", 32, 32);

eq("File \"gpr_3549_test.res\", line 17, characters 5-12", others, [
      0.0,
      0.0,
      1.0,
      1.0,
      2e3
    ]);

Mt.from_pair_suites("Gpr_3549_test", suites.contents);

var u = 32;

var x = 7.0;

var y = 2e3;

var z = 0.2;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.u = u;
exports.x = x;
exports.y = y;
exports.z = z;
exports.others = others;
/*  Not a pure module */

// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + test_id.contents.toString()),
      () => ({
        TAG: "Eq",
        _0: x,
        _1: y
      })
    ],
    tl: suites.contents
  };
}

function f(x) {
  for (let i = 0; i <= 100; ++i) {
    console.log(".");
  }
  return -x | 0;
}

let u = f(-2147483648);

eq("File \"gpr_977_test.res\", line 23, characters 5-12", -2147483648, u);

Mt.from_pair_suites("Gpr_977_test", suites.contents);

let min_32_int = -2147483648;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
exports.min_32_int = min_32_int;
exports.u = u;
/* u Not a pure module */

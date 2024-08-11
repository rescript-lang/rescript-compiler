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
      loc + (" id " + String(test_id.contents)),
      (() => {
        return {
          TAG: "Eq",
          _0: x,
          _1: y
        };
      })
    ],
    tl: suites.contents
  };
}

function f(x) {
  return [
    x._003,
    x._50,
    x._50x,
    x.__50,
    x.__50x,
    x["_50x'"],
    x["x'"]
  ];
}

let v = f({
  _003: 0,
  _50: 1,
  _50x: 2,
  __50: 3,
  __50x: 4,
  "_50x'": 5,
  "x'": 6
});

eq("File \"gpr_1943_test.res\", line 23, characters 3-10", [
  0,
  1,
  2,
  3,
  4,
  5,
  6
], v);

Mt.from_pair_suites("Gpr_1943_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
exports.v = v;
/* v Not a pure module */

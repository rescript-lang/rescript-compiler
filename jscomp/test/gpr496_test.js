// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Primitive_bool = require("../../lib/js/primitive_bool.js");

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

let expected = [
  false,
  false,
  true,
  true,
  -1,
  1,
  0,
  0
];

let expected2 = [
  false,
  false,
  true,
  true,
  -1,
  1,
  0,
  0
];

let u = [
  false,
  false,
  true,
  true,
  -1,
  1,
  0,
  0
];

eq("File \"gpr496_test.res\", line 35, characters 12-19", expected, u);

eq("File \"gpr496_test.res\", line 37, characters 12-19", expected, expected2);

function ff(x, y) {
  return Primitive_bool.min(x, y());
}

eq("File \"gpr496_test.res\", line 40, characters 12-19", true < false ? true : false, false);

Mt.from_pair_suites("Gpr496_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.expected = expected;
exports.expected2 = expected2;
exports.u = u;
exports.ff = ff;
/*  Not a pure module */

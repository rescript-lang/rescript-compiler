// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Runtime_int = require("../../lib/js/runtime_int.js");

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

let a0;

try {
  Runtime_int.div(0, 0);
  a0 = 0;
} catch (exn) {
  a0 = 1;
}

let a1;

try {
  Runtime_int.mod_(0, 0);
  a1 = 0;
} catch (exn$1) {
  a1 = 1;
}

eq("File \"gpr_1760_test.res\", line 26, characters 3-10", [
  a0,
  a1
], [
  1,
  1
]);

Mt.from_pair_suites("Gpr_1760_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.a0 = a0;
exports.a1 = a1;
/* a0 Not a pure module */

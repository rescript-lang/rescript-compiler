// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Curry = require("../../lib/js/curry.js");

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
      (function () {
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

function foo(f) {
  console.log(Curry._2(f, "a1", undefined));
}

foo(function (none, extra) {
  return none + "a2";
});

function foo2(f) {
  return Curry._2(f, "a1", undefined);
}

eq("File \"gpr_1423_app_test.res\", line 15, characters 12-19", "a1a2", "a1a2");

Mt.from_pair_suites("Gpr_1423_app_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.foo = foo;
exports.foo2 = foo2;
/*  Not a pure module */

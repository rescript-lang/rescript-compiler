'use strict';

var Mt = require("./mt.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function foo(a){return a()}
;

function fn(param) {
  console.log("hi");
  return 1;
}

eq("File \"gpr_3492_test.ml\", line 14, characters 6-13", foo((function () {
            console.log("hi");
            return 1;
          })), 1);

Mt.from_pair_suites("gpr_3492_test.ml", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.fn = fn;
/*  Not a pure module */

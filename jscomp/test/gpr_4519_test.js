'use strict';

var Mt = require("./mt.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function nextFor(x) {
  if (x !== undefined) {
    if (x) {
      return ;
    } else {
      return /* Optional */1;
    }
  } else {
    return /* Required */0;
  }
}

eq("File \"gpr_4519_test.ml\", line 17, characters 6-13", /* Optional */1, /* Optional */1);

Mt.from_pair_suites("Gpr_4519_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.nextFor = nextFor;
/*  Not a pure module */

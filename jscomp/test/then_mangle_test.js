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

function then(a, b) {
  console.log("no inline");
  return Math.imul(a, a) + Math.imul(b, b) | 0;
}

eq("File \"then_mangle_test.res\", line 14, characters 3-10", then(1, 2), 5);

Mt.from_pair_suites("then_mangle_test.res", suites.contents);

/*  Not a pure module */

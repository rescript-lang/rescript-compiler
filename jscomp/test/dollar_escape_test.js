'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */{
    _0: /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    _1: suites.contents
  };
  
}

function $$(x, y) {
  return x + y | 0;
}

function $$$plus(x, y) {
  return Math.imul(x, y);
}

eq("File \"dollar_escape_test.ml\", line 20, characters 6-13", 3, 3);

eq("File \"dollar_escape_test.ml\", line 21, characters 6-13", 3, 3);

Mt.from_pair_suites("Dollar_escape_test", suites.contents);

var v = 3;

var u = 3;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.$$ = $$;
exports.v = v;
exports.$$$plus = $$$plus;
exports.u = u;
/*  Not a pure module */

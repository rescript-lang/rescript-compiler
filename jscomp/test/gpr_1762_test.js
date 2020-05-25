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

var v = {
  contents: 3
};

function update(param) {
  v.contents = v.contents + 1 | 0;
  return true;
}

v.contents = v.contents + 1 | 0;

eq("File \"gpr_1762_test.ml\", line 22, characters 6-13", v.contents, 4);

Mt.from_pair_suites("Gpr_1762_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.v = v;
exports.update = update;
/*  Not a pure module */

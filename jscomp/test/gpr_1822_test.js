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

var myShape = {
  tag: /* Circle */0,
  _0: 10
};

var area;

area = myShape.tag ? Math.imul(10, myShape._1) : 100 * 3.14;

eq("File \"gpr_1822_test.ml\", line 21, characters 6-13", area, 314);

Mt.from_pair_suites("Gpr_1822_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.myShape = myShape;
exports.area = area;
/* area Not a pure module */

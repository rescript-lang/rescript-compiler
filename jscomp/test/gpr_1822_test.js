'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Pervasives = require("../../lib/js/pervasives.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
  Pervasives.incr(test_id);
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites.contents
  ];
  return /* () */0;
}

var myShape = /* Circle */Block.__(0, [10]);

var area;

area = myShape.tag ? Caml_int32.imul(10, myShape[1]) : 100 * 3.14;

eq("File \"gpr_1822_test.ml\", line 21, characters 6-13", area, 314);

Mt.from_pair_suites("Gpr_1822_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.myShape = myShape;
exports.area = area;
/* area Not a pure module */

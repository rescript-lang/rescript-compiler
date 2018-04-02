'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function () {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

var expected = /* tuple */[
  false,
  false,
  true,
  true,
  -1,
  1,
  0,
  0
];

var expected2 = /* tuple */[
  false,
  false,
  true,
  true,
  -1,
  1,
  0,
  0
];

var u = /* tuple */[
  false,
  false,
  true,
  true,
  -1,
  1,
  0,
  0
];

eq("File \"gpr496_test.ml\", line 42, characters 12-19", expected, u);

eq("File \"gpr496_test.ml\", line 44, characters 12-19", expected, expected2);

function ff(x, y) {
  return Caml_primitive.caml_bool_min(x, Curry._1(y, /* () */0));
}

eq("File \"gpr496_test.ml\", line 48, characters 5-12", true < false ? true : false, false);

Mt.from_pair_suites("gpr496_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.expected = expected;
exports.expected2 = expected2;
exports.u = u;
exports.ff = ff;
/* expected Not a pure module */

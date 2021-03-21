'use strict';

var Mt = require("./mt.js");
var Caml = require("../../lib/js/caml.js");
var Curry = require("../../lib/js/curry.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    tl: suites.contents
  };
  
}

var expected = [
  false,
  false,
  true,
  true,
  -1,
  1,
  0,
  0
];

var expected2 = [
  false,
  false,
  true,
  true,
  -1,
  1,
  0,
  0
];

var u = [
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
  return Caml.caml_bool_min(x, Curry._1(y, undefined));
}

eq("File \"gpr496_test.ml\", line 48, characters 5-12", true < false ? true : false, false);

Mt.from_pair_suites("Gpr496_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.expected = expected;
exports.expected2 = expected2;
exports.u = u;
exports.ff = ff;
/* expected Not a pure module */

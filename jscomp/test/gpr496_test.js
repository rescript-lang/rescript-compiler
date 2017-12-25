'use strict';

var Mt             = require("./mt.js");
var Block          = require("../../lib/js/block.js");
var Caml_obj       = require("../../lib/js/caml_obj.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
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

var expected_004 = Caml_primitive.caml_int_compare(/* false */0, /* true */1);

var expected_005 = Caml_primitive.caml_int_compare(/* true */1, /* false */0);

var expected_006 = Caml_primitive.caml_int_compare(/* false */0, /* false */0);

var expected_007 = Caml_primitive.caml_int_compare(/* true */1, /* true */1);

var expected = /* tuple */[
  /* false */0,
  /* false */0,
  /* true */1,
  /* true */1,
  expected_004,
  expected_005,
  expected_006,
  expected_007
];

var u_000 = Caml_obj.caml_equal(true, false);

var u_001 = Caml_obj.caml_equal(false, true);

var u_002 = Caml_obj.caml_equal(false, false);

var u_003 = Caml_obj.caml_equal(true, true);

var u_004 = Caml_obj.caml_compare(false, true);

var u_005 = Caml_obj.caml_compare(true, false);

var u_006 = Caml_obj.caml_compare(false, false);

var u_007 = Caml_obj.caml_compare(true, true);

var u = /* tuple */[
  u_000,
  u_001,
  u_002,
  u_003,
  u_004,
  u_005,
  u_006,
  u_007
];

eq("File \"gpr496_test.ml\", line 32, characters 12-19", expected, u);

Mt.from_pair_suites("gpr496_test.ml", suites[0]);

exports.suites   = suites;
exports.test_id  = test_id;
exports.eq       = eq;
exports.expected = expected;
exports.u        = u;
/* expected Not a pure module */

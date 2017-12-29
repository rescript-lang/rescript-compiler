'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_types = require("../../lib/js/js_types.js");

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

eq("File \"gpr_1658_test.ml\", line 11, characters 7-14", null, null);

var match = Js_types.reify_type(null);

if (match[0] !== 1) {
  eq("File \"gpr_1658_test.ml\", line 16, characters 11-18", /* true */1, /* false */0);
} else {
  eq("File \"gpr_1658_test.ml\", line 14, characters 11-18", /* true */1, /* true */1);
}

eq("File \"gpr_1658_test.ml\", line 17, characters 7-14", /* true */1, Js_types.test(null, /* Null */1));

Mt.from_pair_suites("File \"gpr_1658_test.ml\", line 19, characters 22-29", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */

'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_types = require("../../lib/js/js_types.js");
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

eq("File \"gpr_1658_test.ml\", line 11, characters 7-14", null, null);

var match = Js_types.classify(null);

if (typeof match === "number" && match === 2) {
  eq("File \"gpr_1658_test.ml\", line 14, characters 11-18", true, true);
} else {
  eq("File \"gpr_1658_test.ml\", line 16, characters 11-18", true, false);
}

eq("File \"gpr_1658_test.ml\", line 17, characters 7-14", true, Js_types.test(null, /* Null */1));

Mt.from_pair_suites("File \"gpr_1658_test.ml\", line 19, characters 22-29", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */

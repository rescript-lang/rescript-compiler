'use strict';

var Mt = require("./mt.js");
var Js_types = require("../../lib/js/js_types.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */{
    _0: [
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

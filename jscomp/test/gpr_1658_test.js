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
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: "Eq",
                  _0: x,
                  _1: y
                };
        })
    ],
    tl: suites.contents
  };
}

eq("File \"gpr_1658_test.res\", line 10, characters 5-12", null, null);

var match = Js_types.classify(null);

if (typeof match !== "object" && match === "JSNull") {
  eq("File \"gpr_1658_test.res\", line 12, characters 17-24", true, true);
} else {
  eq("File \"gpr_1658_test.res\", line 13, characters 12-19", true, false);
}

eq("File \"gpr_1658_test.res\", line 15, characters 5-12", true, Js_types.test(null, "Null"));

Mt.from_pair_suites("File \"gpr_1658_test.res\", line 18, characters 20-27", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */

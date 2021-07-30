'use strict';

var Mt = require("./mt.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function showToJs(x) {
  return x !== 0;
}

Mt.eq_suites(test_id, suites, "File \"gpr_4900_test.ml\", line 15, characters 30-37", showToJs(/* Yes */1), true);

Mt.eq_suites(test_id, suites, "File \"gpr_4900_test.ml\", line 16, characters 30-37", showToJs(/* No */0), false);

Mt.eq_suites(test_id, suites, "File \"gpr_4900_test.ml\", line 17, characters 30-37", showToJs(/* After */{
          _0: 3
        }), true);

Mt.from_pair_suites("File \"gpr_4900_test.ml\", line 19, characters 20-27", suites.contents);

var from_pair_suites = Mt.from_pair_suites;

var eq_suites = Mt.eq_suites;

exports.from_pair_suites = from_pair_suites;
exports.eq_suites = eq_suites;
exports.suites = suites;
exports.test_id = test_id;
exports.showToJs = showToJs;
/*  Not a pure module */

'use strict';

var Mt = require("./mt.js");

var test_id = {
  contents: 0
};

var suites = {
  contents: /* [] */0
};

function test(x) {
  if (x.NAME === 2) {
    return {
            NAME: 2,
            VAL: x.VAL
          };
  } else {
    return {
            NAME: 1,
            VAL: x.VAL
          };
  }
}

Mt.eq_suites(test_id, suites, "File \"gpr_5218_test.res\", line 11, characters 27-34", test({
          NAME: 1,
          VAL: 3
        }), {
      NAME: 1,
      VAL: 3
    });

Mt.eq_suites(test_id, suites, "File \"gpr_5218_test.res\", line 13, characters 27-34", test({
          NAME: 2,
          VAL: 3
        }), {
      NAME: 2,
      VAL: 3
    });

Mt.from_pair_suites("gpr_5218_test.res", suites.contents);

var eq_suites = Mt.eq_suites;

exports.eq_suites = eq_suites;
exports.test_id = test_id;
exports.suites = suites;
exports.test = test;
/*  Not a pure module */

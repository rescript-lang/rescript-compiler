'use strict';

var Mt = require("./mt.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function fake_c2(a_type, b_type) {
  switch (a_type) {
    case "number" :
        if (b_type === "number") {
          return 33;
        }
        break;
    case "string" :
        return 1;
    case "undefined" :
        return -1;
    default:
      
  }
  if (b_type === "undefined") {
    return 1;
  } else if (a_type === "number") {
    return 3;
  } else {
    return 0;
  }
}

eq("File \"gpr_2931_test.ml\", line 19, characters 6-13", 3, fake_c2("number", "xx"));

Mt.from_pair_suites("Gpr_2931_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.fake_c2 = fake_c2;
/*  Not a pure module */

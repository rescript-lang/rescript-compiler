'use strict';

var Mt = require("./mt.js");
var Int32 = require("../../lib/js/int32.js");

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

function f(x) {
  for(var i = 0; i <= 100; ++i){
    console.log(".");
  }
  return -x | 0;
}

function int32_f(x) {
  for(var i = 0; i <= 100; ++i){
    console.log(".");
  }
  return -x | 0;
}

var u = f(-2147483648);

eq("File \"gpr_977_test.ml\", line 27, characters 5-12", -2147483648, u);

eq("File \"gpr_977_test.ml\", line 28, characters 5-12", Int32.min_int, int32_f(Int32.min_int));

Mt.from_pair_suites("Gpr_977_test", suites.contents);

var min_32_int = -2147483648;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
exports.int32_f = int32_f;
exports.min_32_int = min_32_int;
exports.u = u;
/* u Not a pure module */

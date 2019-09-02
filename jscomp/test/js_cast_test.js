'use strict';

var Mt = require("./mt.js");

var suites = /* record */[/* contents */"[]"];

var counter = /* record */[/* contents */0];

function add_test(loc, test) {
  counter[0] = counter[0] + 1 | 0;
  var id = loc + (" id " + String(counter[0]));
  suites[0] = /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      id,
      test
    ],
    Arg1: suites[0]
  };
  return /* () */0;
}

function eq(loc, x, y) {
  return add_test(loc, (function (param) {
                return /* constructor */{
                        tag: "Eq",
                        Arg0: x,
                        Arg1: y
                      };
              }));
}

eq("File \"js_cast_test.ml\", line 13, characters 12-19", true, 1);

eq("File \"js_cast_test.ml\", line 15, characters 12-19", false, 0);

eq("File \"js_cast_test.ml\", line 17, characters 12-19", 0, 0.0);

eq("File \"js_cast_test.ml\", line 19, characters 12-19", 1, 1.0);

eq("File \"js_cast_test.ml\", line 21, characters 12-19", 123456789, 123456789.0);

Mt.from_pair_suites("Js_cast_test", suites[0]);

exports.suites = suites;
exports.add_test = add_test;
exports.eq = eq;
/*  Not a pure module */

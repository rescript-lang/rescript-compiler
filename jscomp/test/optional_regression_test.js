'use strict';

var Mt = require("./mt.js");
var Js_primitive = require("../../lib/js/js_primitive.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function make(s, b, i) {
  return (function () {
      var tmp = { };
      if (s !== undefined) {
        tmp.s = Js_primitive.valFromOption(s);
      }
      if (b !== undefined) {
        tmp.b = Js_primitive.valFromOption(b);
      }
      if (i !== undefined) {
        tmp.i = Js_primitive.valFromOption(i);
      }
      return tmp;
    });
}

var hh = make("", false, 0)(/* () */0);

eq("File \"optional_regression_test.ml\", line 21, characters 6-13", Js_primitive.undefined_to_opt(hh.s), "");

eq("File \"optional_regression_test.ml\", line 22, characters 6-13", Js_primitive.undefined_to_opt(hh.b), false);

eq("File \"optional_regression_test.ml\", line 23, characters 6-13", Js_primitive.undefined_to_opt(hh.i), 0);

console.log(hh);

Mt.from_pair_suites("optional_regression_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.make = make;
exports.hh = hh;
/* hh Not a pure module */

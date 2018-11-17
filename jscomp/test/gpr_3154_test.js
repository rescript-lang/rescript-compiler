'use strict';

var Mt = require("./mt.js");
var Js_dict = require("../../lib/js/js_dict.js");
var Js_primitive = require("../../lib/js/js_primitive.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

var d = { };

d["foo"] = undefined;

var match = Js_dict.get(d, "foo");

if (match !== undefined && Js_primitive.valFromOption(match) === undefined) {
  b("File \"gpr_3154_test.ml\", line 12, characters 19-26", true);
} else {
  b("File \"gpr_3154_test.ml\", line 13, characters 11-18", false);
}

var d0 = { };

d0["foo"] = undefined;

eq("File \"gpr_3154_test.ml\", line 18, characters 5-12", Js_dict.get(d0, "foo"), Js_primitive.some(undefined));

Mt.from_pair_suites("gpr_3154_test.ml", suites[0]);

var J = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.J = J;
/*  Not a pure module */

'use strict';

var Mt = require("./mt.js");
var Caml_array = require("../../lib/js/caml_array.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function f0(x) {
  var newrecord = Caml_array.caml_array_dup(x);
  newrecord[/* x0 */0] = 1;
  return newrecord;
}

Mt.from_pair_suites("large_record_duplication_test.ml", suites[0]);

var v0 = /* record */[
  /* x0 */9,
  /* x1 */9,
  /* x2 */9,
  /* x3 */9,
  /* x4 */9,
  /* x5 */9,
  /* x6 */9,
  /* x7 */9,
  /* x8 */9,
  /* x9 */9,
  /* x10 */9,
  /* x11 */9,
  /* x12 */9,
  /* x13 */9,
  /* x14 */9,
  /* x15 */9,
  /* x16 */9,
  /* x17 */9,
  /* x18 */9,
  /* x19 */9,
  /* x20 */9,
  /* x21 */9,
  /* x22 */9
];

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.v0 = v0;
exports.f0 = f0;
/*  Not a pure module */

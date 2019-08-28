'use strict';

var Mt = require("./mt.js");
var Caml_weak = require("../../lib/js/caml_weak.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

eq("File \"gpr_2789_test.ml\", line 8, characters 5-12", 0, Caml_weak.caml_weak_create(0).length);

eq("File \"gpr_2789_test.ml\", line 9, characters 5-12", 1, Caml_weak.caml_weak_create(1).length);

Mt.from_pair_suites("Gpr_2789_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */

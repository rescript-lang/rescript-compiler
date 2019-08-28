'use strict';

var Mt = require("./mt.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Gpr_3566_test = require("./gpr_3566_test.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var H = Gpr_3566_test.Test({ });

eq("File \"gpr_3566_drive_test.ml\", line 8, characters 5-12", H.b, true);

var Caml_option$1 = { };

function f(x) {
  return Caml_option.some(x);
}

Mt.from_pair_suites("gpr_3566_drive_test.ml", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.H = H;
exports.Caml_option = Caml_option$1;
exports.f = f;
/* H Not a pure module */

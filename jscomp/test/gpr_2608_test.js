'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var oppHeroes = /* :: */[
  0,
  /* [] */0
];

var huntGrootCondition = /* false */0;

if (List.length(/* [] */0) > 0) {
  var x = List.filter((function () {
            return +(List.hd(/* [] */0) <= 1000);
          }))(oppHeroes);
  huntGrootCondition = +(List.length(x) === 0);
}

var huntGrootCondition2 = /* true */1;

if (List.length(/* [] */0) < 0) {
  var x$1 = List.filter((function () {
            return +(List.hd(/* [] */0) <= 1000);
          }))(oppHeroes);
  huntGrootCondition2 = +(List.length(x$1) === 0);
}

eq("File \"gpr_2608_test.ml\", line 23, characters 5-12", huntGrootCondition, /* false */0);

eq("File \"gpr_2608_test.ml\", line 24, characters 5-12", huntGrootCondition2, /* true */1);

Mt.from_pair_suites("gpr_2608_test.ml", suites[0]);

var nearestGroots = /* [] */0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.nearestGroots = nearestGroots;
exports.oppHeroes = oppHeroes;
exports.huntGrootCondition = huntGrootCondition;
exports.huntGrootCondition2 = huntGrootCondition2;
/* huntGrootCondition Not a pure module */

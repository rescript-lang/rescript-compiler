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

var tmp = /* false */0;

if (List.length(/* [] */0) > 0) {
  var x = List.filter((function () {
            return +(List.hd(/* [] */0) <= 1000);
          }))(oppHeroes);
  tmp = +(List.length(x) === 0);
}

var huntGrootCondition = tmp;

eq("File \"gpr_2608_test.ml\", line 17, characters 5-12", huntGrootCondition, /* false */0);

Mt.from_pair_suites("gpr_2608_test.ml", suites[0]);

var nearestGroots = /* [] */0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.nearestGroots = nearestGroots;
exports.oppHeroes = oppHeroes;
exports.huntGrootCondition = huntGrootCondition;
/* huntGrootCondition Not a pure module */

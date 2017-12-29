'use strict';

var Mt = require("./mt.js");
var Bs_Range = require("../../lib/js/bs_Range.js");
var Bs_SetIntM = require("../../lib/js/bs_SetIntM.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

var v = [Bs_SetIntM.empty];

for(var i = 0; i <= 100000; ++i){
  v[0] = Bs_SetIntM.add(v[0], i);
}

b("File \"bs_mutable_set_test.ml\", line 19, characters 4-11", Bs_SetIntM.checkInvariant(v[0]));

b("File \"bs_mutable_set_test.ml\", line 20, characters 4-11", Bs_Range.forAll(0, 100000, (function (i) {
            return Bs_SetIntM.mem(v[0], i);
          })));

console.log(Bs_SetIntM.length(v[0]));

Mt.from_pair_suites("bs_mutable_set_test.ml", suites[0]);

var N = 0;

var I = 0;

var R = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.N = N;
exports.I = I;
exports.R = R;
/*  Not a pure module */

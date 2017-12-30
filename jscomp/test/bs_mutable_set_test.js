'use strict';

var Mt = require("./mt.js");
var Bs_Array = require("../../lib/js/bs_Array.js");
var Bs_Range = require("../../lib/js/bs_Range.js");
var Bs_SetIntM = require("../../lib/js/bs_SetIntM.js");
var Array_data_util = require("./array_data_util.js");

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

eq("File \"bs_mutable_set_test.ml\", line 23, characters 5-12", Bs_SetIntM.length(v[0]), 100001);

var u = Bs_Array.append(Array_data_util.randomRange(30, 100), Array_data_util.randomRange(40, 120));

var v$1 = Bs_SetIntM.empty;

var v$2 = Bs_SetIntM.addArray(v$1, u);

eq("File \"bs_mutable_set_test.ml\", line 29, characters 5-12", Bs_SetIntM.length(v$2), 91);

eq("File \"bs_mutable_set_test.ml\", line 30, characters 5-12", Bs_SetIntM.toArray(v$2), Array_data_util.range(30, 120));

var u$1 = Bs_Array.append(Array_data_util.randomRange(0, 100000), Array_data_util.randomRange(0, 100));

var v$3 = Bs_SetIntM.ofArray(u$1);

eq("File \"bs_mutable_set_test.ml\", line 35, characters 5-12", Bs_SetIntM.length(v$3), 100001);

var u$2 = Array_data_util.randomRange(50000, 80000);

var v$4 = v$3;

for(var i$1 = 0 ,i_finish = u$2.length - 1 | 0; i$1 <= i_finish; ++i$1){
  v$4 = Bs_SetIntM.remove(v$4, i$1);
}

var v$5 = v$4;

eq("File \"bs_mutable_set_test.ml\", line 42, characters 5-12", Bs_SetIntM.length(v$5), 70000);

Mt.from_pair_suites("bs_mutable_set_test.ml", suites[0]);

var N = 0;

var I = 0;

var R = 0;

var A = 0;

var $plus$plus = Bs_Array.append;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.N = N;
exports.I = I;
exports.R = R;
exports.A = A;
exports.$plus$plus = $plus$plus;
/*  Not a pure module */

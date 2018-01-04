'use strict';

var Mt = require("./mt.js");
var Bs_Array = require("../../lib/js/bs_Array.js");
var Bs_Range = require("../../lib/js/bs_Range.js");
var Bs_SetIntM = require("../../lib/js/bs_SetIntM.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Array_data_util = require("./array_data_util.js");
var Bs_internalAVLset = require("../../lib/js/bs_internalAVLset.js");
var Bs_internalSetInt = require("../../lib/js/bs_internalSetInt.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

var v = {
  data: Bs_internalAVLset.empty0
};

for(var i = 0; i <= 100000; ++i){
  Bs_SetIntM.addOnly(v, i);
}

b("File \"bs_mutable_set_test.ml\", line 19, characters 4-11", Bs_internalAVLset.checkInvariant(v.data));

b("File \"bs_mutable_set_test.ml\", line 20, characters 4-11", Bs_Range.forAll(0, 100000, (function (i) {
            return Bs_internalSetInt.mem(v.data, i);
          })));

eq("File \"bs_mutable_set_test.ml\", line 23, characters 5-12", Bs_internalAVLset.length0(v.data), 100001);

var u = Bs_Array.append(Array_data_util.randomRange(30, 100), Array_data_util.randomRange(40, 120));

var v$1 = {
  data: Bs_internalAVLset.empty0
};

Bs_SetIntM.addArrayOnly(v$1, u);

eq("File \"bs_mutable_set_test.ml\", line 29, characters 5-12", Bs_internalAVLset.length0(v$1.data), 91);

eq("File \"bs_mutable_set_test.ml\", line 30, characters 5-12", Bs_internalAVLset.toArray0(v$1.data), Array_data_util.range(30, 120));

var u$1 = Bs_Array.append(Array_data_util.randomRange(0, 100000), Array_data_util.randomRange(0, 100));

var v$2 = {
  data: Bs_internalSetInt.ofArray(u$1)
};

eq("File \"bs_mutable_set_test.ml\", line 35, characters 5-12", Bs_internalAVLset.length0(v$2.data), 100001);

var u$2 = Array_data_util.randomRange(50000, 80000);

for(var i$1 = 0 ,i_finish = u$2.length - 1 | 0; i$1 <= i_finish; ++i$1){
  Bs_SetIntM.removeOnly(v$2, i$1);
}

eq("File \"bs_mutable_set_test.ml\", line 42, characters 5-12", Bs_internalAVLset.length0(v$2.data), 70000);

var vv = Array_data_util.randomRange(0, 100000);

for(var i$2 = 0 ,i_finish$1 = vv.length - 1 | 0; i$2 <= i_finish$1; ++i$2){
  Bs_SetIntM.removeOnly(v$2, Caml_array.caml_array_get(vv, i$2));
}

eq("File \"bs_mutable_set_test.ml\", line 48, characters 5-12", Bs_internalAVLset.length0(v$2.data), 0);

b("File \"bs_mutable_set_test.ml\", line 49, characters 4-11", Bs_internalAVLset.isEmpty0(v$2.data));

var xs = Bs_Array.init(30, (function (i) {
        return i;
      }));

var v$3 = {
  data: Bs_internalSetInt.ofArray(xs)
};

Bs_SetIntM.removeOnly(v$3, 30);

Bs_SetIntM.removeOnly(v$3, 29);

b("File \"bs_mutable_set_test.ml\", line 55, characters 4-11", +(28 === Bs_internalAVLset.maxNull0(v$3.data)));

Bs_SetIntM.removeOnly(v$3, 0);

b("File \"bs_mutable_set_test.ml\", line 57, characters 4-11", +(1 === Bs_internalAVLset.minNull0(v$3.data)));

eq("File \"bs_mutable_set_test.ml\", line 58, characters 5-12", Bs_internalAVLset.length0(v$3.data), 28);

var vv$1 = Array_data_util.randomRange(1, 28);

for(var i$3 = 0 ,i_finish$2 = vv$1.length - 1 | 0; i$3 <= i_finish$2; ++i$3){
  Bs_SetIntM.removeOnly(v$3, Caml_array.caml_array_get(vv$1, i$3));
}

eq("File \"bs_mutable_set_test.ml\", line 63, characters 5-12", Bs_internalAVLset.length0(v$3.data), 0);

function id(loc, x) {
  var u = {
    data: Bs_internalAVLset.ofSortedArrayUnsafe0(x)
  };
  b(loc, Bs_internalAVLset.checkInvariant(u.data));
  return b(loc, Bs_Array.forAll2(Bs_internalAVLset.toArray0(u.data), x, (function (x, y) {
                    return +(x === y);
                  })));
}

id("File \"bs_mutable_set_test.ml\", line 71, characters 5-12", /* int array */[]);

id("File \"bs_mutable_set_test.ml\", line 72, characters 5-12", /* int array */[0]);

id("File \"bs_mutable_set_test.ml\", line 73, characters 5-12", /* int array */[
      0,
      1
    ]);

id("File \"bs_mutable_set_test.ml\", line 74, characters 5-12", /* int array */[
      0,
      1,
      2
    ]);

id("File \"bs_mutable_set_test.ml\", line 75, characters 5-12", /* int array */[
      0,
      1,
      2,
      3
    ]);

id("File \"bs_mutable_set_test.ml\", line 76, characters 5-12", /* array */[
      0,
      1,
      2,
      3,
      4
    ]);

id("File \"bs_mutable_set_test.ml\", line 77, characters 5-12", /* array */[
      0,
      1,
      2,
      3,
      4,
      5
    ]);

id("File \"bs_mutable_set_test.ml\", line 78, characters 5-12", /* array */[
      0,
      1,
      2,
      3,
      4,
      6
    ]);

id("File \"bs_mutable_set_test.ml\", line 79, characters 5-12", /* array */[
      0,
      1,
      2,
      3,
      4,
      6,
      7
    ]);

id("File \"bs_mutable_set_test.ml\", line 80, characters 5-12", /* array */[
      0,
      1,
      2,
      3,
      4,
      6,
      7,
      8
    ]);

id("File \"bs_mutable_set_test.ml\", line 81, characters 5-12", /* array */[
      0,
      1,
      2,
      3,
      4,
      6,
      7,
      8,
      9
    ]);

id("File \"bs_mutable_set_test.ml\", line 82, characters 5-12", Array_data_util.range(0, 1000));

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

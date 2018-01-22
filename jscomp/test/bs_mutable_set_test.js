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
  Bs_SetIntM.addDone(v, i);
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

Bs_SetIntM.mergeArrayDone(v$1, u);

eq("File \"bs_mutable_set_test.ml\", line 29, characters 5-12", Bs_internalAVLset.length0(v$1.data), 91);

eq("File \"bs_mutable_set_test.ml\", line 30, characters 5-12", Bs_internalAVLset.toArray0(v$1.data), Array_data_util.range(30, 120));

var u$1 = Bs_Array.append(Array_data_util.randomRange(0, 100000), Array_data_util.randomRange(0, 100));

var v$2 = {
  data: Bs_internalSetInt.ofArray(u$1)
};

eq("File \"bs_mutable_set_test.ml\", line 35, characters 5-12", Bs_internalAVLset.length0(v$2.data), 100001);

var u$2 = Array_data_util.randomRange(50000, 80000);

for(var i$1 = 0 ,i_finish = u$2.length - 1 | 0; i$1 <= i_finish; ++i$1){
  Bs_SetIntM.removeDone(v$2, i$1);
}

eq("File \"bs_mutable_set_test.ml\", line 42, characters 5-12", Bs_internalAVLset.length0(v$2.data), 70000);

var vv = Array_data_util.randomRange(0, 100000);

for(var i$2 = 0 ,i_finish$1 = vv.length - 1 | 0; i$2 <= i_finish$1; ++i$2){
  Bs_SetIntM.removeDone(v$2, Caml_array.caml_array_get(vv, i$2));
}

eq("File \"bs_mutable_set_test.ml\", line 48, characters 5-12", Bs_internalAVLset.length0(v$2.data), 0);

b("File \"bs_mutable_set_test.ml\", line 49, characters 4-11", Bs_internalAVLset.isEmpty0(v$2.data));

var xs = Bs_Array.init(30, (function (i) {
        return i;
      }));

var v$3 = {
  data: Bs_internalSetInt.ofArray(xs)
};

Bs_SetIntM.removeDone(v$3, 30);

Bs_SetIntM.removeDone(v$3, 29);

b("File \"bs_mutable_set_test.ml\", line 55, characters 4-11", +(28 === Bs_internalAVLset.maxNull0(v$3.data)));

Bs_SetIntM.removeDone(v$3, 0);

b("File \"bs_mutable_set_test.ml\", line 57, characters 4-11", +(1 === Bs_internalAVLset.minNull0(v$3.data)));

eq("File \"bs_mutable_set_test.ml\", line 58, characters 5-12", Bs_internalAVLset.length0(v$3.data), 28);

var vv$1 = Array_data_util.randomRange(1, 28);

for(var i$3 = 0 ,i_finish$2 = vv$1.length - 1 | 0; i$3 <= i_finish$2; ++i$3){
  Bs_SetIntM.removeDone(v$3, Caml_array.caml_array_get(vv$1, i$3));
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

var xs$1 = Array_data_util.randomRange(0, 1000);

var v$4 = {
  data: Bs_internalSetInt.ofArray(xs$1)
};

var copyV = Bs_SetIntM.filter(v$4, (function (x) {
        return +(x % 8 === 0);
      }));

var match = Bs_SetIntM.partition(v$4, (function (x) {
        return +(x % 8 === 0);
      }));

var cc = Bs_SetIntM.filter(v$4, (function (x) {
        return +(x % 8 !== 0);
      }));

for(var i$4 = 0; i$4 <= 200; ++i$4){
  Bs_SetIntM.removeDone(v$4, i$4);
}

eq("File \"bs_mutable_set_test.ml\", line 92, characters 5-12", Bs_internalAVLset.length0(copyV.data), 126);

eq("File \"bs_mutable_set_test.ml\", line 93, characters 5-12", Bs_internalAVLset.toArray0(copyV.data), Bs_Array.init(126, (function (i) {
            return (i << 3);
          })));

eq("File \"bs_mutable_set_test.ml\", line 94, characters 5-12", Bs_internalAVLset.length0(v$4.data), 800);

b("File \"bs_mutable_set_test.ml\", line 95, characters 4-11", Bs_SetIntM.eq(copyV, match[0]));

b("File \"bs_mutable_set_test.ml\", line 96, characters 4-11", Bs_SetIntM.eq(cc, match[1]));

var xs$2 = Array_data_util.randomRange(0, 1000);

var v$5 = {
  data: Bs_internalSetInt.ofArray(xs$2)
};

var match$1 = Bs_SetIntM.split(v$5, 400);

var match$2 = match$1[0];

var xs$3 = Array_data_util.randomRange(0, 399);

b("File \"bs_mutable_set_test.ml\", line 101, characters 4-11", Bs_SetIntM.eq(match$2[0], {
          data: Bs_internalSetInt.ofArray(xs$3)
        }));

var xs$4 = Array_data_util.randomRange(401, 1000);

b("File \"bs_mutable_set_test.ml\", line 102, characters 4-11", Bs_SetIntM.eq(match$2[1], {
          data: Bs_internalSetInt.ofArray(xs$4)
        }));

var xs$5 = Bs_Array.map(Array_data_util.randomRange(0, 1000), (function (x) {
        return (x << 1);
      }));

var d = {
  data: Bs_internalSetInt.ofArray(xs$5)
};

var match$3 = Bs_SetIntM.split(d, 1001);

var match$4 = match$3[0];

var xs$6 = Bs_Array.init(501, (function (x) {
        return (x << 1);
      }));

b("File \"bs_mutable_set_test.ml\", line 105, characters 4-11", Bs_SetIntM.eq(match$4[0], {
          data: Bs_internalSetInt.ofArray(xs$6)
        }));

var xs$7 = Bs_Array.init(500, (function (x) {
        return 1002 + (x << 1) | 0;
      }));

b("File \"bs_mutable_set_test.ml\", line 106, characters 4-11", Bs_SetIntM.eq(match$4[1], {
          data: Bs_internalSetInt.ofArray(xs$7)
        }));

var xs$8 = Array_data_util.randomRange(0, 100);

var aa = {
  data: Bs_internalSetInt.ofArray(xs$8)
};

var xs$9 = Array_data_util.randomRange(40, 120);

var bb = {
  data: Bs_internalSetInt.ofArray(xs$9)
};

var cc$1 = Bs_SetIntM.union(aa, bb);

var xs$10 = Array_data_util.randomRange(0, 120);

b("File \"bs_mutable_set_test.ml\", line 116, characters 4-11", Bs_SetIntM.eq(cc$1, {
          data: Bs_internalSetInt.ofArray(xs$10)
        }));

var xs$11 = Array_data_util.randomRange(0, 20);

var xs$12 = Array_data_util.randomRange(21, 40);

var xs$13 = Array_data_util.randomRange(0, 40);

b("File \"bs_mutable_set_test.ml\", line 118, characters 4-11", Bs_SetIntM.eq(Bs_SetIntM.union({
              data: Bs_internalSetInt.ofArray(xs$11)
            }, {
              data: Bs_internalSetInt.ofArray(xs$12)
            }), {
          data: Bs_internalSetInt.ofArray(xs$13)
        }));

var dd = Bs_SetIntM.inter(aa, bb);

var xs$14 = Array_data_util.randomRange(40, 100);

b("File \"bs_mutable_set_test.ml\", line 123, characters 4-11", Bs_SetIntM.eq(dd, {
          data: Bs_internalSetInt.ofArray(xs$14)
        }));

var xs$15 = Array_data_util.randomRange(0, 20);

var xs$16 = Array_data_util.randomRange(21, 40);

b("File \"bs_mutable_set_test.ml\", line 124, characters 4-11", Bs_SetIntM.eq(Bs_SetIntM.inter({
              data: Bs_internalSetInt.ofArray(xs$15)
            }, {
              data: Bs_internalSetInt.ofArray(xs$16)
            }), {
          data: Bs_internalAVLset.empty0
        }));

var xs$17 = Array_data_util.randomRange(21, 40);

var xs$18 = Array_data_util.randomRange(0, 20);

b("File \"bs_mutable_set_test.ml\", line 130, characters 4-11", Bs_SetIntM.eq(Bs_SetIntM.inter({
              data: Bs_internalSetInt.ofArray(xs$17)
            }, {
              data: Bs_internalSetInt.ofArray(xs$18)
            }), {
          data: Bs_internalAVLset.empty0
        }));

b("File \"bs_mutable_set_test.ml\", line 136, characters 4-11", Bs_SetIntM.eq(Bs_SetIntM.inter({
              data: Bs_internalSetInt.ofArray(/* array */[
                    1,
                    3,
                    4,
                    5,
                    7,
                    9
                  ])
            }, {
              data: Bs_internalSetInt.ofArray(/* array */[
                    2,
                    4,
                    5,
                    6,
                    8,
                    10
                  ])
            }), {
          data: Bs_internalSetInt.ofArray(/* int array */[
                4,
                5
              ])
        }));

var xs$19 = Array_data_util.randomRange(0, 39);

b("File \"bs_mutable_set_test.ml\", line 142, characters 4-11", Bs_SetIntM.eq(Bs_SetIntM.diff(aa, bb), {
          data: Bs_internalSetInt.ofArray(xs$19)
        }));

var xs$20 = Array_data_util.randomRange(101, 120);

b("File \"bs_mutable_set_test.ml\", line 144, characters 4-11", Bs_SetIntM.eq(Bs_SetIntM.diff(bb, aa), {
          data: Bs_internalSetInt.ofArray(xs$20)
        }));

var xs$21 = Array_data_util.randomRange(21, 40);

var xs$22 = Array_data_util.randomRange(0, 20);

var xs$23 = Array_data_util.randomRange(21, 40);

b("File \"bs_mutable_set_test.ml\", line 146, characters 4-11", Bs_SetIntM.eq(Bs_SetIntM.diff({
              data: Bs_internalSetInt.ofArray(xs$21)
            }, {
              data: Bs_internalSetInt.ofArray(xs$22)
            }), {
          data: Bs_internalSetInt.ofArray(xs$23)
        }));

var xs$24 = Array_data_util.randomRange(0, 20);

var xs$25 = Array_data_util.randomRange(21, 40);

var xs$26 = Array_data_util.randomRange(0, 20);

b("File \"bs_mutable_set_test.ml\", line 152, characters 4-11", Bs_SetIntM.eq(Bs_SetIntM.diff({
              data: Bs_internalSetInt.ofArray(xs$24)
            }, {
              data: Bs_internalSetInt.ofArray(xs$25)
            }), {
          data: Bs_internalSetInt.ofArray(xs$26)
        }));

var xs$27 = Array_data_util.randomRange(0, 20);

var xs$28 = Array_data_util.randomRange(0, 40);

var xs$29 = Array_data_util.randomRange(0, -1);

b("File \"bs_mutable_set_test.ml\", line 159, characters 4-11", Bs_SetIntM.eq(Bs_SetIntM.diff({
              data: Bs_internalSetInt.ofArray(xs$27)
            }, {
              data: Bs_internalSetInt.ofArray(xs$28)
            }), {
          data: Bs_internalSetInt.ofArray(xs$29)
        }));

Mt.from_pair_suites("bs_mutable_set_test.ml", suites[0]);

var N = 0;

var I = 0;

var R = 0;

var A = 0;

var $plus$plus = Bs_SetIntM.union;

var f = Bs_SetIntM.ofArray;

var $eq$tilde = Bs_SetIntM.eq;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.N = N;
exports.I = I;
exports.R = R;
exports.A = A;
exports.$plus$plus = $plus$plus;
exports.f = f;
exports.$eq$tilde = $eq$tilde;
/*  Not a pure module */

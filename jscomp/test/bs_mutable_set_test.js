'use strict';

var Mt = require("./mt.js");
var Belt_List = require("../../lib/js/belt_List.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Belt_Range = require("../../lib/js/belt_Range.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Array_data_util = require("./array_data_util.js");
var Belt_MutableSetInt = require("../../lib/js/belt_MutableSetInt.js");
var Belt_internalAVLset = require("../../lib/js/belt_internalAVLset.js");
var Belt_internalSetInt = require("../../lib/js/belt_internalSetInt.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

var xs = Array_data_util.range(0, 30);

var u = {
  data: Belt_internalSetInt.fromArray(xs)
};

b("File \"bs_mutable_set_test.ml\", line 20, characters 4-11", Belt_MutableSetInt.removeCheck(u, 0));

b("File \"bs_mutable_set_test.ml\", line 21, characters 4-11", !Belt_MutableSetInt.removeCheck(u, 0));

b("File \"bs_mutable_set_test.ml\", line 22, characters 4-11", Belt_MutableSetInt.removeCheck(u, 30));

b("File \"bs_mutable_set_test.ml\", line 23, characters 4-11", Belt_MutableSetInt.removeCheck(u, 20));

eq("File \"bs_mutable_set_test.ml\", line 24, characters 5-12", Belt_internalAVLset.size(u.data), 28);

var r = Array_data_util.randomRange(0, 30);

b("File \"bs_mutable_set_test.ml\", line 26, characters 4-11", 29 === Belt_internalAVLset.maxUndefined(u.data));

b("File \"bs_mutable_set_test.ml\", line 27, characters 4-11", 1 === Belt_internalAVLset.minUndefined(u.data));

Belt_MutableSetInt.add(u, 3);

for(var i = 0 ,i_finish = r.length - 1 | 0; i <= i_finish; ++i){
  Belt_MutableSetInt.remove(u, r[i]);
}

b("File \"bs_mutable_set_test.ml\", line 32, characters 4-11", Belt_MutableSetInt.isEmpty(u));

Belt_MutableSetInt.add(u, 0);

Belt_MutableSetInt.add(u, 1);

Belt_MutableSetInt.add(u, 2);

Belt_MutableSetInt.add(u, 0);

eq("File \"bs_mutable_set_test.ml\", line 37, characters 5-12", Belt_internalAVLset.size(u.data), 3);

b("File \"bs_mutable_set_test.ml\", line 38, characters 4-11", !Belt_MutableSetInt.isEmpty(u));

for(var i$1 = 0; i$1 <= 3; ++i$1){
  Belt_MutableSetInt.remove(u, i$1);
}

b("File \"bs_mutable_set_test.ml\", line 42, characters 4-11", Belt_MutableSetInt.isEmpty(u));

Belt_MutableSetInt.mergeMany(u, Array_data_util.randomRange(0, 20000));

Belt_MutableSetInt.mergeMany(u, Array_data_util.randomRange(0, 200));

eq("File \"bs_mutable_set_test.ml\", line 45, characters 5-12", Belt_internalAVLset.size(u.data), 20001);

Belt_MutableSetInt.removeMany(u, Array_data_util.randomRange(0, 200));

eq("File \"bs_mutable_set_test.ml\", line 47, characters 5-12", Belt_internalAVLset.size(u.data), 19800);

Belt_MutableSetInt.removeMany(u, Array_data_util.randomRange(0, 1000));

eq("File \"bs_mutable_set_test.ml\", line 49, characters 5-12", Belt_internalAVLset.size(u.data), 19000);

Belt_MutableSetInt.removeMany(u, Array_data_util.randomRange(0, 1000));

eq("File \"bs_mutable_set_test.ml\", line 51, characters 5-12", Belt_internalAVLset.size(u.data), 19000);

Belt_MutableSetInt.removeMany(u, Array_data_util.randomRange(1000, 10000));

eq("File \"bs_mutable_set_test.ml\", line 53, characters 5-12", Belt_internalAVLset.size(u.data), 10000);

Belt_MutableSetInt.removeMany(u, Array_data_util.randomRange(10000, 19999));

eq("File \"bs_mutable_set_test.ml\", line 55, characters 5-12", Belt_internalAVLset.size(u.data), 1);

b("File \"bs_mutable_set_test.ml\", line 56, characters 4-11", Belt_internalSetInt.has(u.data, 20000));

Belt_MutableSetInt.removeMany(u, Array_data_util.randomRange(10000, 30000));

b("File \"bs_mutable_set_test.ml\", line 58, characters 4-11", Belt_MutableSetInt.isEmpty(u));

var xs$1 = Array_data_util.randomRange(1000, 2000);

var v = {
  data: Belt_internalSetInt.fromArray(xs$1)
};

var bs = Belt_Array.map(Array_data_util.randomRange(500, 1499), (function (x) {
        return Belt_MutableSetInt.removeCheck(v, x);
      }));

var indeedRemoved = Belt_Array.reduce(bs, 0, (function (acc, x) {
        if (x) {
          return acc + 1 | 0;
        } else {
          return acc;
        }
      }));

eq("File \"bs_mutable_set_test.ml\", line 65, characters 5-12", indeedRemoved, 500);

eq("File \"bs_mutable_set_test.ml\", line 66, characters 5-12", Belt_internalAVLset.size(v.data), 501);

var cs = Belt_Array.map(Array_data_util.randomRange(500, 2000), (function (x) {
        return Belt_MutableSetInt.addCheck(v, x);
      }));

var indeedAded = Belt_Array.reduce(cs, 0, (function (acc, x) {
        if (x) {
          return acc + 1 | 0;
        } else {
          return acc;
        }
      }));

eq("File \"bs_mutable_set_test.ml\", line 69, characters 5-12", indeedAded, 1000);

eq("File \"bs_mutable_set_test.ml\", line 70, characters 5-12", Belt_internalAVLset.size(v.data), 1501);

b("File \"bs_mutable_set_test.ml\", line 71, characters 4-11", Belt_MutableSetInt.isEmpty({
          data: Belt_internalAVLset.empty
        }));

eq("File \"bs_mutable_set_test.ml\", line 72, characters 5-12", Belt_internalAVLset.minimum(v.data), 500);

eq("File \"bs_mutable_set_test.ml\", line 73, characters 5-12", Belt_internalAVLset.maximum(v.data), 2000);

eq("File \"bs_mutable_set_test.ml\", line 74, characters 5-12", Belt_internalAVLset.minUndefined(v.data), 500);

eq("File \"bs_mutable_set_test.ml\", line 75, characters 5-12", Belt_internalAVLset.maxUndefined(v.data), 2000);

eq("File \"bs_mutable_set_test.ml\", line 76, characters 5-12", Belt_MutableSetInt.reduce(v, 0, (function (x, y) {
            return x + y | 0;
          })), 1876250);

b("File \"bs_mutable_set_test.ml\", line 77, characters 4-11", Belt_List.eq(Belt_internalAVLset.toList(v.data), Belt_List.makeBy(1501, (function (i) {
                return i + 500 | 0;
              })), (function (x, y) {
            return x === y;
          })));

eq("File \"bs_mutable_set_test.ml\", line 78, characters 5-12", Belt_internalAVLset.toArray(v.data), Array_data_util.range(500, 2000));

Belt_internalAVLset.checkInvariantInternal(v.data);

eq("File \"bs_mutable_set_test.ml\", line 80, characters 5-12", Belt_internalSetInt.get(v.data, 3), undefined);

eq("File \"bs_mutable_set_test.ml\", line 81, characters 5-12", Belt_internalSetInt.get(v.data, 1200), 1200);

var match = Belt_MutableSetInt.split(v, 1000);

var match$1 = match[0];

var bb = match$1[1];

var aa = match$1[0];

b("File \"bs_mutable_set_test.ml\", line 83, characters 4-11", match[1]);

b("File \"bs_mutable_set_test.ml\", line 84, characters 4-11", Belt_Array.eq(Belt_internalAVLset.toArray(aa.data), Array_data_util.range(500, 999), (function (x, y) {
            return x === y;
          })));

b("File \"bs_mutable_set_test.ml\", line 85, characters 4-11", Belt_Array.eq(Belt_internalAVLset.toArray(bb.data), Array_data_util.range(1001, 2000), (function (prim, prim$1) {
            return prim === prim$1;
          })));

b("File \"bs_mutable_set_test.ml\", line 86, characters 5-12", Belt_MutableSetInt.subset(aa, v));

b("File \"bs_mutable_set_test.ml\", line 87, characters 4-11", Belt_MutableSetInt.subset(bb, v));

b("File \"bs_mutable_set_test.ml\", line 88, characters 4-11", Belt_MutableSetInt.isEmpty(Belt_MutableSetInt.intersect(aa, bb)));

var c = Belt_MutableSetInt.removeCheck(v, 1000);

b("File \"bs_mutable_set_test.ml\", line 90, characters 4-11", c);

var match$2 = Belt_MutableSetInt.split(v, 1000);

var match$3 = match$2[0];

var bb$1 = match$3[1];

var aa$1 = match$3[0];

b("File \"bs_mutable_set_test.ml\", line 92, characters 4-11", !match$2[1]);

b("File \"bs_mutable_set_test.ml\", line 93, characters 4-11", Belt_Array.eq(Belt_internalAVLset.toArray(aa$1.data), Array_data_util.range(500, 999), (function (prim, prim$1) {
            return prim === prim$1;
          })));

b("File \"bs_mutable_set_test.ml\", line 94, characters 4-11", Belt_Array.eq(Belt_internalAVLset.toArray(bb$1.data), Array_data_util.range(1001, 2000), (function (prim, prim$1) {
            return prim === prim$1;
          })));

b("File \"bs_mutable_set_test.ml\", line 95, characters 5-12", Belt_MutableSetInt.subset(aa$1, v));

b("File \"bs_mutable_set_test.ml\", line 96, characters 4-11", Belt_MutableSetInt.subset(bb$1, v));

b("File \"bs_mutable_set_test.ml\", line 97, characters 4-11", Belt_MutableSetInt.isEmpty(Belt_MutableSetInt.intersect(aa$1, bb$1)));

var xs$2 = Array_data_util.randomRange(0, 100);

var aa$2 = {
  data: Belt_internalSetInt.fromArray(xs$2)
};

var xs$3 = Array_data_util.randomRange(40, 120);

var bb$2 = {
  data: Belt_internalSetInt.fromArray(xs$3)
};

var cc = Belt_MutableSetInt.union(aa$2, bb$2);

var xs$4 = Array_data_util.randomRange(0, 120);

b("File \"bs_mutable_set_test.ml\", line 106, characters 4-11", Belt_MutableSetInt.eq(cc, {
          data: Belt_internalSetInt.fromArray(xs$4)
        }));

var xs$5 = Array_data_util.randomRange(0, 20);

var xs$6 = Array_data_util.randomRange(21, 40);

var xs$7 = Array_data_util.randomRange(0, 40);

b("File \"bs_mutable_set_test.ml\", line 108, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.union({
              data: Belt_internalSetInt.fromArray(xs$5)
            }, {
              data: Belt_internalSetInt.fromArray(xs$6)
            }), {
          data: Belt_internalSetInt.fromArray(xs$7)
        }));

var dd = Belt_MutableSetInt.intersect(aa$2, bb$2);

var xs$8 = Array_data_util.randomRange(40, 100);

b("File \"bs_mutable_set_test.ml\", line 113, characters 4-11", Belt_MutableSetInt.eq(dd, {
          data: Belt_internalSetInt.fromArray(xs$8)
        }));

var xs$9 = Array_data_util.randomRange(0, 20);

var xs$10 = Array_data_util.randomRange(21, 40);

b("File \"bs_mutable_set_test.ml\", line 114, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.intersect({
              data: Belt_internalSetInt.fromArray(xs$9)
            }, {
              data: Belt_internalSetInt.fromArray(xs$10)
            }), {
          data: Belt_internalAVLset.empty
        }));

var xs$11 = Array_data_util.randomRange(21, 40);

var xs$12 = Array_data_util.randomRange(0, 20);

b("File \"bs_mutable_set_test.ml\", line 120, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.intersect({
              data: Belt_internalSetInt.fromArray(xs$11)
            }, {
              data: Belt_internalSetInt.fromArray(xs$12)
            }), {
          data: Belt_internalAVLset.empty
        }));

b("File \"bs_mutable_set_test.ml\", line 126, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.intersect({
              data: Belt_internalSetInt.fromArray(/* array */[
                    1,
                    3,
                    4,
                    5,
                    7,
                    9
                  ])
            }, {
              data: Belt_internalSetInt.fromArray(/* array */[
                    2,
                    4,
                    5,
                    6,
                    8,
                    10
                  ])
            }), {
          data: Belt_internalSetInt.fromArray(/* array */[
                4,
                5
              ])
        }));

var xs$13 = Array_data_util.randomRange(0, 39);

b("File \"bs_mutable_set_test.ml\", line 132, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff(aa$2, bb$2), {
          data: Belt_internalSetInt.fromArray(xs$13)
        }));

var xs$14 = Array_data_util.randomRange(101, 120);

b("File \"bs_mutable_set_test.ml\", line 134, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff(bb$2, aa$2), {
          data: Belt_internalSetInt.fromArray(xs$14)
        }));

var xs$15 = Array_data_util.randomRange(21, 40);

var xs$16 = Array_data_util.randomRange(0, 20);

var xs$17 = Array_data_util.randomRange(21, 40);

b("File \"bs_mutable_set_test.ml\", line 136, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff({
              data: Belt_internalSetInt.fromArray(xs$15)
            }, {
              data: Belt_internalSetInt.fromArray(xs$16)
            }), {
          data: Belt_internalSetInt.fromArray(xs$17)
        }));

var xs$18 = Array_data_util.randomRange(0, 20);

var xs$19 = Array_data_util.randomRange(21, 40);

var xs$20 = Array_data_util.randomRange(0, 20);

b("File \"bs_mutable_set_test.ml\", line 142, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff({
              data: Belt_internalSetInt.fromArray(xs$18)
            }, {
              data: Belt_internalSetInt.fromArray(xs$19)
            }), {
          data: Belt_internalSetInt.fromArray(xs$20)
        }));

var xs$21 = Array_data_util.randomRange(0, 20);

var xs$22 = Array_data_util.randomRange(0, 40);

var xs$23 = Array_data_util.randomRange(0, -1);

b("File \"bs_mutable_set_test.ml\", line 149, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff({
              data: Belt_internalSetInt.fromArray(xs$21)
            }, {
              data: Belt_internalSetInt.fromArray(xs$22)
            }), {
          data: Belt_internalSetInt.fromArray(xs$23)
        }));

var xs$24 = Array_data_util.randomRange(0, 1000);

var a0 = {
  data: Belt_internalSetInt.fromArray(xs$24)
};

var a1 = Belt_MutableSetInt.keep(a0, (function (x) {
        return x % 2 === 0;
      }));

var a2 = Belt_MutableSetInt.keep(a0, (function (x) {
        return x % 2 !== 0;
      }));

var match$4 = Belt_MutableSetInt.partition(a0, (function (x) {
        return x % 2 === 0;
      }));

var a4 = match$4[1];

var a3 = match$4[0];

b("File \"bs_mutable_set_test.ml\", line 164, characters 4-11", Belt_MutableSetInt.eq(a1, a3));

b("File \"bs_mutable_set_test.ml\", line 165, characters 4-11", Belt_MutableSetInt.eq(a2, a4));

Belt_List.forEach(/* :: */[
      a0,
      /* :: */[
        a1,
        /* :: */[
          a2,
          /* :: */[
            a3,
            /* :: */[
              a4,
              /* [] */0
            ]
          ]
        ]
      ]
    ], (function (x) {
        return Belt_internalAVLset.checkInvariantInternal(x.data);
      }));

var v$1 = {
  data: Belt_internalAVLset.empty
};

for(var i$2 = 0; i$2 <= 100000; ++i$2){
  Belt_MutableSetInt.add(v$1, i$2);
}

Belt_internalAVLset.checkInvariantInternal(v$1.data);

b("File \"bs_mutable_set_test.ml\", line 178, characters 4-11", Belt_Range.every(0, 100000, (function (i) {
            return Belt_internalSetInt.has(v$1.data, i);
          })));

eq("File \"bs_mutable_set_test.ml\", line 181, characters 5-12", Belt_internalAVLset.size(v$1.data), 100001);

var u$1 = Belt_Array.concat(Array_data_util.randomRange(30, 100), Array_data_util.randomRange(40, 120));

var v$2 = {
  data: Belt_internalAVLset.empty
};

Belt_MutableSetInt.mergeMany(v$2, u$1);

eq("File \"bs_mutable_set_test.ml\", line 187, characters 5-12", Belt_internalAVLset.size(v$2.data), 91);

eq("File \"bs_mutable_set_test.ml\", line 188, characters 5-12", Belt_internalAVLset.toArray(v$2.data), Array_data_util.range(30, 120));

var u$2 = Belt_Array.concat(Array_data_util.randomRange(0, 100000), Array_data_util.randomRange(0, 100));

var v$3 = {
  data: Belt_internalSetInt.fromArray(u$2)
};

eq("File \"bs_mutable_set_test.ml\", line 193, characters 5-12", Belt_internalAVLset.size(v$3.data), 100001);

var u$3 = Array_data_util.randomRange(50000, 80000);

for(var i$3 = 0 ,i_finish$1 = u$3.length - 1 | 0; i$3 <= i_finish$1; ++i$3){
  Belt_MutableSetInt.remove(v$3, i$3);
}

eq("File \"bs_mutable_set_test.ml\", line 200, characters 5-12", Belt_internalAVLset.size(v$3.data), 70000);

var vv = Array_data_util.randomRange(0, 100000);

for(var i$4 = 0 ,i_finish$2 = vv.length - 1 | 0; i$4 <= i_finish$2; ++i$4){
  Belt_MutableSetInt.remove(v$3, Caml_array.caml_array_get(vv, i$4));
}

eq("File \"bs_mutable_set_test.ml\", line 206, characters 5-12", Belt_internalAVLset.size(v$3.data), 0);

b("File \"bs_mutable_set_test.ml\", line 207, characters 4-11", Belt_MutableSetInt.isEmpty(v$3));

var xs$25 = Belt_Array.makeBy(30, (function (i) {
        return i;
      }));

var v$4 = {
  data: Belt_internalSetInt.fromArray(xs$25)
};

Belt_MutableSetInt.remove(v$4, 30);

Belt_MutableSetInt.remove(v$4, 29);

b("File \"bs_mutable_set_test.ml\", line 213, characters 4-11", 28 === Belt_internalAVLset.maxUndefined(v$4.data));

Belt_MutableSetInt.remove(v$4, 0);

b("File \"bs_mutable_set_test.ml\", line 215, characters 4-11", 1 === Belt_internalAVLset.minUndefined(v$4.data));

eq("File \"bs_mutable_set_test.ml\", line 216, characters 5-12", Belt_internalAVLset.size(v$4.data), 28);

var vv$1 = Array_data_util.randomRange(1, 28);

for(var i$5 = 0 ,i_finish$3 = vv$1.length - 1 | 0; i$5 <= i_finish$3; ++i$5){
  Belt_MutableSetInt.remove(v$4, Caml_array.caml_array_get(vv$1, i$5));
}

eq("File \"bs_mutable_set_test.ml\", line 221, characters 5-12", Belt_internalAVLset.size(v$4.data), 0);

function id(loc, x) {
  var u = {
    data: Belt_internalAVLset.fromSortedArrayUnsafe(x)
  };
  Belt_internalAVLset.checkInvariantInternal(u.data);
  return b(loc, Belt_Array.every2(Belt_internalAVLset.toArray(u.data), x, (function (prim, prim$1) {
                    return prim === prim$1;
                  })));
}

id("File \"bs_mutable_set_test.ml\", line 229, characters 5-12", /* array */[]);

id("File \"bs_mutable_set_test.ml\", line 230, characters 5-12", /* array */[0]);

id("File \"bs_mutable_set_test.ml\", line 231, characters 5-12", /* array */[
      0,
      1
    ]);

id("File \"bs_mutable_set_test.ml\", line 232, characters 5-12", /* array */[
      0,
      1,
      2
    ]);

id("File \"bs_mutable_set_test.ml\", line 233, characters 5-12", /* array */[
      0,
      1,
      2,
      3
    ]);

id("File \"bs_mutable_set_test.ml\", line 234, characters 5-12", /* array */[
      0,
      1,
      2,
      3,
      4
    ]);

id("File \"bs_mutable_set_test.ml\", line 235, characters 5-12", /* array */[
      0,
      1,
      2,
      3,
      4,
      5
    ]);

id("File \"bs_mutable_set_test.ml\", line 236, characters 5-12", /* array */[
      0,
      1,
      2,
      3,
      4,
      6
    ]);

id("File \"bs_mutable_set_test.ml\", line 237, characters 5-12", /* array */[
      0,
      1,
      2,
      3,
      4,
      6,
      7
    ]);

id("File \"bs_mutable_set_test.ml\", line 238, characters 5-12", /* array */[
      0,
      1,
      2,
      3,
      4,
      6,
      7,
      8
    ]);

id("File \"bs_mutable_set_test.ml\", line 239, characters 5-12", /* array */[
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

id("File \"bs_mutable_set_test.ml\", line 240, characters 5-12", Array_data_util.range(0, 1000));

var xs$26 = Array_data_util.randomRange(0, 1000);

var v$5 = {
  data: Belt_internalSetInt.fromArray(xs$26)
};

var copyV = Belt_MutableSetInt.keep(v$5, (function (x) {
        return x % 8 === 0;
      }));

var match$5 = Belt_MutableSetInt.partition(v$5, (function (x) {
        return x % 8 === 0;
      }));

var cc$1 = Belt_MutableSetInt.keep(v$5, (function (x) {
        return x % 8 !== 0;
      }));

for(var i$6 = 0; i$6 <= 200; ++i$6){
  Belt_MutableSetInt.remove(v$5, i$6);
}

eq("File \"bs_mutable_set_test.ml\", line 250, characters 5-12", Belt_internalAVLset.size(copyV.data), 126);

eq("File \"bs_mutable_set_test.ml\", line 251, characters 5-12", Belt_internalAVLset.toArray(copyV.data), Belt_Array.makeBy(126, (function (i) {
            return (i << 3);
          })));

eq("File \"bs_mutable_set_test.ml\", line 252, characters 5-12", Belt_internalAVLset.size(v$5.data), 800);

b("File \"bs_mutable_set_test.ml\", line 253, characters 4-11", Belt_MutableSetInt.eq(copyV, match$5[0]));

b("File \"bs_mutable_set_test.ml\", line 254, characters 4-11", Belt_MutableSetInt.eq(cc$1, match$5[1]));

var xs$27 = Array_data_util.randomRange(0, 1000);

var v$6 = {
  data: Belt_internalSetInt.fromArray(xs$27)
};

var match$6 = Belt_MutableSetInt.split(v$6, 400);

var match$7 = match$6[0];

var xs$28 = Array_data_util.randomRange(0, 399);

b("File \"bs_mutable_set_test.ml\", line 259, characters 4-11", Belt_MutableSetInt.eq(match$7[0], {
          data: Belt_internalSetInt.fromArray(xs$28)
        }));

var xs$29 = Array_data_util.randomRange(401, 1000);

b("File \"bs_mutable_set_test.ml\", line 260, characters 4-11", Belt_MutableSetInt.eq(match$7[1], {
          data: Belt_internalSetInt.fromArray(xs$29)
        }));

var xs$30 = Belt_Array.map(Array_data_util.randomRange(0, 1000), (function (x) {
        return (x << 1);
      }));

var d = {
  data: Belt_internalSetInt.fromArray(xs$30)
};

var match$8 = Belt_MutableSetInt.split(d, 1001);

var match$9 = match$8[0];

var xs$31 = Belt_Array.makeBy(501, (function (x) {
        return (x << 1);
      }));

b("File \"bs_mutable_set_test.ml\", line 263, characters 4-11", Belt_MutableSetInt.eq(match$9[0], {
          data: Belt_internalSetInt.fromArray(xs$31)
        }));

var xs$32 = Belt_Array.makeBy(500, (function (x) {
        return 1002 + (x << 1) | 0;
      }));

b("File \"bs_mutable_set_test.ml\", line 264, characters 4-11", Belt_MutableSetInt.eq(match$9[1], {
          data: Belt_internalSetInt.fromArray(xs$32)
        }));

var xs$33 = Array_data_util.randomRange(0, 100);

var aa$3 = {
  data: Belt_internalSetInt.fromArray(xs$33)
};

var xs$34 = Array_data_util.randomRange(40, 120);

var bb$3 = {
  data: Belt_internalSetInt.fromArray(xs$34)
};

var cc$2 = Belt_MutableSetInt.union(aa$3, bb$3);

var xs$35 = Array_data_util.randomRange(0, 120);

b("File \"bs_mutable_set_test.ml\", line 274, characters 4-11", Belt_MutableSetInt.eq(cc$2, {
          data: Belt_internalSetInt.fromArray(xs$35)
        }));

var xs$36 = Array_data_util.randomRange(0, 20);

var xs$37 = Array_data_util.randomRange(21, 40);

var xs$38 = Array_data_util.randomRange(0, 40);

b("File \"bs_mutable_set_test.ml\", line 276, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.union({
              data: Belt_internalSetInt.fromArray(xs$36)
            }, {
              data: Belt_internalSetInt.fromArray(xs$37)
            }), {
          data: Belt_internalSetInt.fromArray(xs$38)
        }));

var dd$1 = Belt_MutableSetInt.intersect(aa$3, bb$3);

var xs$39 = Array_data_util.randomRange(40, 100);

b("File \"bs_mutable_set_test.ml\", line 281, characters 4-11", Belt_MutableSetInt.eq(dd$1, {
          data: Belt_internalSetInt.fromArray(xs$39)
        }));

var xs$40 = Array_data_util.randomRange(0, 20);

var xs$41 = Array_data_util.randomRange(21, 40);

b("File \"bs_mutable_set_test.ml\", line 282, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.intersect({
              data: Belt_internalSetInt.fromArray(xs$40)
            }, {
              data: Belt_internalSetInt.fromArray(xs$41)
            }), {
          data: Belt_internalAVLset.empty
        }));

var xs$42 = Array_data_util.randomRange(21, 40);

var xs$43 = Array_data_util.randomRange(0, 20);

b("File \"bs_mutable_set_test.ml\", line 288, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.intersect({
              data: Belt_internalSetInt.fromArray(xs$42)
            }, {
              data: Belt_internalSetInt.fromArray(xs$43)
            }), {
          data: Belt_internalAVLset.empty
        }));

b("File \"bs_mutable_set_test.ml\", line 294, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.intersect({
              data: Belt_internalSetInt.fromArray(/* array */[
                    1,
                    3,
                    4,
                    5,
                    7,
                    9
                  ])
            }, {
              data: Belt_internalSetInt.fromArray(/* array */[
                    2,
                    4,
                    5,
                    6,
                    8,
                    10
                  ])
            }), {
          data: Belt_internalSetInt.fromArray(/* array */[
                4,
                5
              ])
        }));

var xs$44 = Array_data_util.randomRange(0, 39);

b("File \"bs_mutable_set_test.ml\", line 300, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff(aa$3, bb$3), {
          data: Belt_internalSetInt.fromArray(xs$44)
        }));

var xs$45 = Array_data_util.randomRange(101, 120);

b("File \"bs_mutable_set_test.ml\", line 302, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff(bb$3, aa$3), {
          data: Belt_internalSetInt.fromArray(xs$45)
        }));

var xs$46 = Array_data_util.randomRange(21, 40);

var xs$47 = Array_data_util.randomRange(0, 20);

var xs$48 = Array_data_util.randomRange(21, 40);

b("File \"bs_mutable_set_test.ml\", line 304, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff({
              data: Belt_internalSetInt.fromArray(xs$46)
            }, {
              data: Belt_internalSetInt.fromArray(xs$47)
            }), {
          data: Belt_internalSetInt.fromArray(xs$48)
        }));

var xs$49 = Array_data_util.randomRange(0, 20);

var xs$50 = Array_data_util.randomRange(21, 40);

var xs$51 = Array_data_util.randomRange(0, 20);

b("File \"bs_mutable_set_test.ml\", line 310, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff({
              data: Belt_internalSetInt.fromArray(xs$49)
            }, {
              data: Belt_internalSetInt.fromArray(xs$50)
            }), {
          data: Belt_internalSetInt.fromArray(xs$51)
        }));

var xs$52 = Array_data_util.randomRange(0, 20);

var xs$53 = Array_data_util.randomRange(0, 40);

var xs$54 = Array_data_util.randomRange(0, -1);

b("File \"bs_mutable_set_test.ml\", line 317, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff({
              data: Belt_internalSetInt.fromArray(xs$52)
            }, {
              data: Belt_internalSetInt.fromArray(xs$53)
            }), {
          data: Belt_internalSetInt.fromArray(xs$54)
        }));

Mt.from_pair_suites("Bs_mutable_set_test", suites[0]);

var N = 0;

var I = 0;

var R = 0;

var A = 0;

var L = 0;

var empty = Belt_MutableSetInt.make;

var fromArray = Belt_MutableSetInt.fromArray;

var $plus$plus = Belt_MutableSetInt.union;

var f = Belt_MutableSetInt.fromArray;

var $eq$tilde = Belt_MutableSetInt.eq;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.N = N;
exports.I = I;
exports.R = R;
exports.A = A;
exports.L = L;
exports.empty = empty;
exports.fromArray = fromArray;
exports.$plus$plus = $plus$plus;
exports.f = f;
exports.$eq$tilde = $eq$tilde;
/* u Not a pure module */

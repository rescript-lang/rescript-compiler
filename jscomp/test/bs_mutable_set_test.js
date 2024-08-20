// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Belt_List = require("../../lib/js/belt_List.js");
let Belt_Array = require("../../lib/js/belt_Array.js");
let Belt_Range = require("../../lib/js/belt_Range.js");
let Caml_array = require("../../lib/js/caml_array.js");
let Array_data_util = require("./array_data_util.js");
let Belt_MutableSetInt = require("../../lib/js/belt_MutableSetInt.js");
let Belt_internalAVLset = require("../../lib/js/belt_internalAVLset.js");
let Belt_internalSetInt = require("../../lib/js/belt_internalSetInt.js");

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  Mt.bool_suites(test_id, suites, loc, x);
}

let xs = Array_data_util.range(0, 30);

let u = {
  data: Belt_internalSetInt.fromArray(xs)
};

b("File \"bs_mutable_set_test.res\", line 21, characters 8-15", Belt_MutableSetInt.removeCheck(u, 0));

b("File \"bs_mutable_set_test.res\", line 22, characters 8-15", !Belt_MutableSetInt.removeCheck(u, 0));

b("File \"bs_mutable_set_test.res\", line 23, characters 8-15", Belt_MutableSetInt.removeCheck(u, 30));

b("File \"bs_mutable_set_test.res\", line 24, characters 8-15", Belt_MutableSetInt.removeCheck(u, 20));

eq("File \"bs_mutable_set_test.res\", line 25, characters 9-16", Belt_internalAVLset.size(u.data), 28);

let r = Array_data_util.randomRange(0, 30);

b("File \"bs_mutable_set_test.res\", line 27, characters 8-15", 29 === Belt_internalAVLset.maxUndefined(u.data));

b("File \"bs_mutable_set_test.res\", line 28, characters 8-15", 1 === Belt_internalAVLset.minUndefined(u.data));

Belt_MutableSetInt.add(u, 3);

for (let i = 0, i_finish = r.length; i < i_finish; ++i) {
  Belt_MutableSetInt.remove(u, r[i]);
}

b("File \"bs_mutable_set_test.res\", line 33, characters 8-15", Belt_MutableSetInt.isEmpty(u));

Belt_MutableSetInt.add(u, 0);

Belt_MutableSetInt.add(u, 1);

Belt_MutableSetInt.add(u, 2);

Belt_MutableSetInt.add(u, 0);

eq("File \"bs_mutable_set_test.res\", line 38, characters 9-16", Belt_internalAVLset.size(u.data), 3);

b("File \"bs_mutable_set_test.res\", line 39, characters 8-15", !Belt_MutableSetInt.isEmpty(u));

for (let i$1 = 0; i$1 <= 3; ++i$1) {
  Belt_MutableSetInt.remove(u, i$1);
}

b("File \"bs_mutable_set_test.res\", line 43, characters 8-15", Belt_MutableSetInt.isEmpty(u));

Belt_MutableSetInt.mergeMany(u, Array_data_util.randomRange(0, 20000));

Belt_MutableSetInt.mergeMany(u, Array_data_util.randomRange(0, 200));

eq("File \"bs_mutable_set_test.res\", line 46, characters 9-16", Belt_internalAVLset.size(u.data), 20001);

Belt_MutableSetInt.removeMany(u, Array_data_util.randomRange(0, 200));

eq("File \"bs_mutable_set_test.res\", line 48, characters 9-16", Belt_internalAVLset.size(u.data), 19800);

Belt_MutableSetInt.removeMany(u, Array_data_util.randomRange(0, 1000));

eq("File \"bs_mutable_set_test.res\", line 50, characters 9-16", Belt_internalAVLset.size(u.data), 19000);

Belt_MutableSetInt.removeMany(u, Array_data_util.randomRange(0, 1000));

eq("File \"bs_mutable_set_test.res\", line 52, characters 9-16", Belt_internalAVLset.size(u.data), 19000);

Belt_MutableSetInt.removeMany(u, Array_data_util.randomRange(1000, 10000));

eq("File \"bs_mutable_set_test.res\", line 54, characters 9-16", Belt_internalAVLset.size(u.data), 10000);

Belt_MutableSetInt.removeMany(u, Array_data_util.randomRange(10000, 19999));

eq("File \"bs_mutable_set_test.res\", line 56, characters 9-16", Belt_internalAVLset.size(u.data), 1);

b("File \"bs_mutable_set_test.res\", line 57, characters 8-15", Belt_internalSetInt.has(u.data, 20000));

Belt_MutableSetInt.removeMany(u, Array_data_util.randomRange(10000, 30000));

b("File \"bs_mutable_set_test.res\", line 59, characters 8-15", Belt_MutableSetInt.isEmpty(u));

let xs$1 = Array_data_util.randomRange(1000, 2000);

let v = {
  data: Belt_internalSetInt.fromArray(xs$1)
};

let bs = Belt_Array.map(Array_data_util.randomRange(500, 1499), (x => Belt_MutableSetInt.removeCheck(v, x)));

let indeedRemoved = Belt_Array.reduce(bs, 0, ((acc, x) => {
  if (x) {
    return acc + 1 | 0;
  } else {
    return acc;
  }
}));

eq("File \"bs_mutable_set_test.res\", line 72, characters 9-16", indeedRemoved, 500);

eq("File \"bs_mutable_set_test.res\", line 73, characters 9-16", Belt_internalAVLset.size(v.data), 501);

let cs = Belt_Array.map(Array_data_util.randomRange(500, 2000), (x => Belt_MutableSetInt.addCheck(v, x)));

let indeedAded = Belt_Array.reduce(cs, 0, ((acc, x) => {
  if (x) {
    return acc + 1 | 0;
  } else {
    return acc;
  }
}));

eq("File \"bs_mutable_set_test.res\", line 82, characters 9-16", indeedAded, 1000);

eq("File \"bs_mutable_set_test.res\", line 83, characters 9-16", Belt_internalAVLset.size(v.data), 1501);

b("File \"bs_mutable_set_test.res\", line 84, characters 8-15", Belt_MutableSetInt.isEmpty({
  data: undefined
}));

eq("File \"bs_mutable_set_test.res\", line 85, characters 9-16", Belt_internalAVLset.minimum(v.data), 500);

eq("File \"bs_mutable_set_test.res\", line 86, characters 9-16", Belt_internalAVLset.maximum(v.data), 2000);

eq("File \"bs_mutable_set_test.res\", line 87, characters 9-16", Belt_internalAVLset.minUndefined(v.data), 500);

eq("File \"bs_mutable_set_test.res\", line 88, characters 9-16", Belt_internalAVLset.maxUndefined(v.data), 2000);

eq("File \"bs_mutable_set_test.res\", line 89, characters 9-16", Belt_MutableSetInt.reduce(v, 0, ((x, y) => x + y | 0)), 1876250);

b("File \"bs_mutable_set_test.res\", line 90, characters 8-15", Belt_List.eq(Belt_internalAVLset.toList(v.data), Belt_List.makeBy(1501, (i => i + 500 | 0)), ((x, y) => x === y)));

eq("File \"bs_mutable_set_test.res\", line 91, characters 9-16", Belt_internalAVLset.toArray(v.data), Array_data_util.range(500, 2000));

Belt_internalAVLset.checkInvariantInternal(v.data);

eq("File \"bs_mutable_set_test.res\", line 93, characters 9-16", Belt_internalSetInt.get(v.data, 3), undefined);

eq("File \"bs_mutable_set_test.res\", line 94, characters 9-16", Belt_internalSetInt.get(v.data, 1200), 1200);

let match = Belt_MutableSetInt.split(v, 1000);

let match$1 = match[0];

let bb = match$1[1];

let aa = match$1[0];

b("File \"bs_mutable_set_test.res\", line 96, characters 8-15", match[1]);

b("File \"bs_mutable_set_test.res\", line 97, characters 8-15", Belt_Array.eq(Belt_internalAVLset.toArray(aa.data), Array_data_util.range(500, 999), ((x, y) => x === y)));

b("File \"bs_mutable_set_test.res\", line 98, characters 8-15", Belt_Array.eq(Belt_internalAVLset.toArray(bb.data), Array_data_util.range(1001, 2000), ((prim0, prim1) => prim0 === prim1)));

b("File \"bs_mutable_set_test.res\", line 99, characters 8-15", Belt_MutableSetInt.subset(aa, v));

b("File \"bs_mutable_set_test.res\", line 100, characters 8-15", Belt_MutableSetInt.subset(bb, v));

b("File \"bs_mutable_set_test.res\", line 101, characters 8-15", Belt_MutableSetInt.isEmpty(Belt_MutableSetInt.intersect(aa, bb)));

let c = Belt_MutableSetInt.removeCheck(v, 1000);

b("File \"bs_mutable_set_test.res\", line 103, characters 8-15", c);

let match$2 = Belt_MutableSetInt.split(v, 1000);

let match$3 = match$2[0];

let bb$1 = match$3[1];

let aa$1 = match$3[0];

b("File \"bs_mutable_set_test.res\", line 105, characters 8-15", !match$2[1]);

b("File \"bs_mutable_set_test.res\", line 106, characters 8-15", Belt_Array.eq(Belt_internalAVLset.toArray(aa$1.data), Array_data_util.range(500, 999), ((prim0, prim1) => prim0 === prim1)));

b("File \"bs_mutable_set_test.res\", line 107, characters 8-15", Belt_Array.eq(Belt_internalAVLset.toArray(bb$1.data), Array_data_util.range(1001, 2000), ((prim0, prim1) => prim0 === prim1)));

b("File \"bs_mutable_set_test.res\", line 108, characters 8-15", Belt_MutableSetInt.subset(aa$1, v));

b("File \"bs_mutable_set_test.res\", line 109, characters 8-15", Belt_MutableSetInt.subset(bb$1, v));

b("File \"bs_mutable_set_test.res\", line 110, characters 8-15", Belt_MutableSetInt.isEmpty(Belt_MutableSetInt.intersect(aa$1, bb$1)));

let xs$2 = Array_data_util.randomRange(0, 100);

let aa$2 = {
  data: Belt_internalSetInt.fromArray(xs$2)
};

let xs$3 = Array_data_util.randomRange(40, 120);

let bb$2 = {
  data: Belt_internalSetInt.fromArray(xs$3)
};

let cc = Belt_MutableSetInt.union(aa$2, bb$2);

let xs$4 = Array_data_util.randomRange(0, 120);

b("File \"bs_mutable_set_test.res\", line 120, characters 8-15", Belt_MutableSetInt.eq(cc, {
  data: Belt_internalSetInt.fromArray(xs$4)
}));

let xs$5 = Array_data_util.randomRange(0, 20);

let xs$6 = Array_data_util.randomRange(21, 40);

let xs$7 = Array_data_util.randomRange(0, 40);

b("File \"bs_mutable_set_test.res\", line 123, characters 8-15", Belt_MutableSetInt.eq(Belt_MutableSetInt.union({
  data: Belt_internalSetInt.fromArray(xs$5)
}, {
  data: Belt_internalSetInt.fromArray(xs$6)
}), {
  data: Belt_internalSetInt.fromArray(xs$7)
}));

let dd = Belt_MutableSetInt.intersect(aa$2, bb$2);

let xs$8 = Array_data_util.randomRange(40, 100);

b("File \"bs_mutable_set_test.res\", line 127, characters 8-15", Belt_MutableSetInt.eq(dd, {
  data: Belt_internalSetInt.fromArray(xs$8)
}));

let xs$9 = Array_data_util.randomRange(0, 20);

let xs$10 = Array_data_util.randomRange(21, 40);

b("File \"bs_mutable_set_test.res\", line 129, characters 8-15", Belt_MutableSetInt.eq(Belt_MutableSetInt.intersect({
  data: Belt_internalSetInt.fromArray(xs$9)
}, {
  data: Belt_internalSetInt.fromArray(xs$10)
}), {
  data: undefined
}));

let xs$11 = Array_data_util.randomRange(21, 40);

let xs$12 = Array_data_util.randomRange(0, 20);

b("File \"bs_mutable_set_test.res\", line 136, characters 8-15", Belt_MutableSetInt.eq(Belt_MutableSetInt.intersect({
  data: Belt_internalSetInt.fromArray(xs$11)
}, {
  data: Belt_internalSetInt.fromArray(xs$12)
}), {
  data: undefined
}));

b("File \"bs_mutable_set_test.res\", line 142, characters 8-15", Belt_MutableSetInt.eq(Belt_MutableSetInt.intersect({
  data: Belt_internalSetInt.fromArray([
    1,
    3,
    4,
    5,
    7,
    9
  ])
}, {
  data: Belt_internalSetInt.fromArray([
    2,
    4,
    5,
    6,
    8,
    10
  ])
}), {
  data: Belt_internalSetInt.fromArray([
    4,
    5
  ])
}));

let xs$13 = Array_data_util.randomRange(0, 39);

b("File \"bs_mutable_set_test.res\", line 143, characters 8-15", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff(aa$2, bb$2), {
  data: Belt_internalSetInt.fromArray(xs$13)
}));

let xs$14 = Array_data_util.randomRange(101, 120);

b("File \"bs_mutable_set_test.res\", line 144, characters 8-15", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff(bb$2, aa$2), {
  data: Belt_internalSetInt.fromArray(xs$14)
}));

let xs$15 = Array_data_util.randomRange(21, 40);

let xs$16 = Array_data_util.randomRange(0, 20);

let xs$17 = Array_data_util.randomRange(21, 40);

b("File \"bs_mutable_set_test.res\", line 146, characters 8-15", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff({
  data: Belt_internalSetInt.fromArray(xs$15)
}, {
  data: Belt_internalSetInt.fromArray(xs$16)
}), {
  data: Belt_internalSetInt.fromArray(xs$17)
}));

let xs$18 = Array_data_util.randomRange(0, 20);

let xs$19 = Array_data_util.randomRange(21, 40);

let xs$20 = Array_data_util.randomRange(0, 20);

b("File \"bs_mutable_set_test.res\", line 153, characters 8-15", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff({
  data: Belt_internalSetInt.fromArray(xs$18)
}, {
  data: Belt_internalSetInt.fromArray(xs$19)
}), {
  data: Belt_internalSetInt.fromArray(xs$20)
}));

let xs$21 = Array_data_util.randomRange(0, 20);

let xs$22 = Array_data_util.randomRange(0, 40);

let xs$23 = Array_data_util.randomRange(0, -1);

b("File \"bs_mutable_set_test.res\", line 161, characters 8-15", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff({
  data: Belt_internalSetInt.fromArray(xs$21)
}, {
  data: Belt_internalSetInt.fromArray(xs$22)
}), {
  data: Belt_internalSetInt.fromArray(xs$23)
}));

let xs$24 = Array_data_util.randomRange(0, 1000);

let a0 = {
  data: Belt_internalSetInt.fromArray(xs$24)
};

let a1 = Belt_MutableSetInt.keep(a0, (x => x % 2 === 0));

let a2 = Belt_MutableSetInt.keep(a0, (x => x % 2 !== 0));

let match$4 = Belt_MutableSetInt.partition(a0, (x => x % 2 === 0));

let a4 = match$4[1];

let a3 = match$4[0];

b("File \"bs_mutable_set_test.res\", line 173, characters 8-15", Belt_MutableSetInt.eq(a1, a3));

b("File \"bs_mutable_set_test.res\", line 174, characters 8-15", Belt_MutableSetInt.eq(a2, a4));

Belt_List.forEach({
  hd: a0,
  tl: {
    hd: a1,
    tl: {
      hd: a2,
      tl: {
        hd: a3,
        tl: {
          hd: a4,
          tl: /* [] */0
        }
      }
    }
  }
}, (x => Belt_internalAVLset.checkInvariantInternal(x.data)));

let v$1 = {
  data: undefined
};

for (let i$2 = 0; i$2 <= 100000; ++i$2) {
  Belt_MutableSetInt.add(v$1, i$2);
}

Belt_internalAVLset.checkInvariantInternal(v$1.data);

b("File \"bs_mutable_set_test.res\", line 188, characters 10-17", Belt_Range.every(0, 100000, (i => Belt_internalSetInt.has(v$1.data, i))));

eq("File \"bs_mutable_set_test.res\", line 189, characters 5-12", Belt_internalAVLset.size(v$1.data), 100001);

let u$1 = Belt_Array.concat(Array_data_util.randomRange(30, 100), Array_data_util.randomRange(40, 120));

let v$2 = {
  data: undefined
};

Belt_MutableSetInt.mergeMany(v$2, u$1);

eq("File \"bs_mutable_set_test.res\", line 196, characters 5-12", Belt_internalAVLset.size(v$2.data), 91);

eq("File \"bs_mutable_set_test.res\", line 197, characters 5-12", Belt_internalAVLset.toArray(v$2.data), Array_data_util.range(30, 120));

let u$2 = Belt_Array.concat(Array_data_util.randomRange(0, 100000), Array_data_util.randomRange(0, 100));

let v$3 = {
  data: Belt_internalSetInt.fromArray(u$2)
};

eq("File \"bs_mutable_set_test.res\", line 203, characters 5-12", Belt_internalAVLset.size(v$3.data), 100001);

let u$3 = Array_data_util.randomRange(50000, 80000);

for (let i$3 = 0, i_finish$1 = u$3.length; i$3 < i_finish$1; ++i$3) {
  Belt_MutableSetInt.remove(v$3, i$3);
}

eq("File \"bs_mutable_set_test.res\", line 210, characters 5-12", Belt_internalAVLset.size(v$3.data), 70000);

let vv = Array_data_util.randomRange(0, 100000);

for (let i$4 = 0, i_finish$2 = vv.length; i$4 < i_finish$2; ++i$4) {
  Belt_MutableSetInt.remove(v$3, Caml_array.get(vv, i$4));
}

eq("File \"bs_mutable_set_test.res\", line 216, characters 5-12", Belt_internalAVLset.size(v$3.data), 0);

b("File \"bs_mutable_set_test.res\", line 217, characters 4-11", Belt_MutableSetInt.isEmpty(v$3));

let xs$25 = Belt_Array.makeBy(30, (i => i));

let v$4 = {
  data: Belt_internalSetInt.fromArray(xs$25)
};

Belt_MutableSetInt.remove(v$4, 30);

Belt_MutableSetInt.remove(v$4, 29);

b("File \"bs_mutable_set_test.res\", line 224, characters 4-11", 28 === Belt_internalAVLset.maxUndefined(v$4.data));

Belt_MutableSetInt.remove(v$4, 0);

b("File \"bs_mutable_set_test.res\", line 226, characters 4-11", 1 === Belt_internalAVLset.minUndefined(v$4.data));

eq("File \"bs_mutable_set_test.res\", line 227, characters 5-12", Belt_internalAVLset.size(v$4.data), 28);

let vv$1 = Array_data_util.randomRange(1, 28);

for (let i$5 = 0, i_finish$3 = vv$1.length; i$5 < i_finish$3; ++i$5) {
  Belt_MutableSetInt.remove(v$4, Caml_array.get(vv$1, i$5));
}

eq("File \"bs_mutable_set_test.res\", line 232, characters 5-12", Belt_internalAVLset.size(v$4.data), 0);

function id(loc, x) {
  let u = {
    data: Belt_internalAVLset.fromSortedArrayUnsafe(x)
  };
  Belt_internalAVLset.checkInvariantInternal(u.data);
  b(loc, Belt_Array.every2(Belt_internalAVLset.toArray(u.data), x, ((prim0, prim1) => prim0 === prim1)));
}

id("File \"bs_mutable_set_test.res\", line 242, characters 5-12", []);

id("File \"bs_mutable_set_test.res\", line 243, characters 5-12", [0]);

id("File \"bs_mutable_set_test.res\", line 244, characters 5-12", [
  0,
  1
]);

id("File \"bs_mutable_set_test.res\", line 245, characters 5-12", [
  0,
  1,
  2
]);

id("File \"bs_mutable_set_test.res\", line 246, characters 5-12", [
  0,
  1,
  2,
  3
]);

id("File \"bs_mutable_set_test.res\", line 247, characters 5-12", [
  0,
  1,
  2,
  3,
  4
]);

id("File \"bs_mutable_set_test.res\", line 248, characters 5-12", [
  0,
  1,
  2,
  3,
  4,
  5
]);

id("File \"bs_mutable_set_test.res\", line 249, characters 5-12", [
  0,
  1,
  2,
  3,
  4,
  6
]);

id("File \"bs_mutable_set_test.res\", line 250, characters 5-12", [
  0,
  1,
  2,
  3,
  4,
  6,
  7
]);

id("File \"bs_mutable_set_test.res\", line 251, characters 5-12", [
  0,
  1,
  2,
  3,
  4,
  6,
  7,
  8
]);

id("File \"bs_mutable_set_test.res\", line 252, characters 5-12", [
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

id("File \"bs_mutable_set_test.res\", line 253, characters 5-12", Array_data_util.range(0, 1000));

let xs$26 = Array_data_util.randomRange(0, 1000);

let v$5 = {
  data: Belt_internalSetInt.fromArray(xs$26)
};

let copyV = Belt_MutableSetInt.keep(v$5, (x => x % 8 === 0));

let match$5 = Belt_MutableSetInt.partition(v$5, (x => x % 8 === 0));

let cc$1 = Belt_MutableSetInt.keep(v$5, (x => x % 8 !== 0));

for (let i$6 = 0; i$6 <= 200; ++i$6) {
  Belt_MutableSetInt.remove(v$5, i$6);
}

eq("File \"bs_mutable_set_test.res\", line 264, characters 5-12", Belt_internalAVLset.size(copyV.data), 126);

eq("File \"bs_mutable_set_test.res\", line 265, characters 5-12", Belt_internalAVLset.toArray(copyV.data), Belt_Array.makeBy(126, (i => (i << 3))));

eq("File \"bs_mutable_set_test.res\", line 266, characters 5-12", Belt_internalAVLset.size(v$5.data), 800);

b("File \"bs_mutable_set_test.res\", line 267, characters 4-11", Belt_MutableSetInt.eq(copyV, match$5[0]));

b("File \"bs_mutable_set_test.res\", line 268, characters 4-11", Belt_MutableSetInt.eq(cc$1, match$5[1]));

let xs$27 = Array_data_util.randomRange(0, 1000);

let v$6 = {
  data: Belt_internalSetInt.fromArray(xs$27)
};

let match$6 = Belt_MutableSetInt.split(v$6, 400);

let match$7 = match$6[0];

let xs$28 = Array_data_util.randomRange(0, 399);

b("File \"bs_mutable_set_test.res\", line 274, characters 4-11", Belt_MutableSetInt.eq(match$7[0], {
  data: Belt_internalSetInt.fromArray(xs$28)
}));

let xs$29 = Array_data_util.randomRange(401, 1000);

b("File \"bs_mutable_set_test.res\", line 275, characters 4-11", Belt_MutableSetInt.eq(match$7[1], {
  data: Belt_internalSetInt.fromArray(xs$29)
}));

let xs$30 = Belt_Array.map(Array_data_util.randomRange(0, 1000), (x => (x << 1)));

let d = {
  data: Belt_internalSetInt.fromArray(xs$30)
};

let match$8 = Belt_MutableSetInt.split(d, 1001);

let match$9 = match$8[0];

let xs$31 = Belt_Array.makeBy(501, (x => (x << 1)));

b("File \"bs_mutable_set_test.res\", line 278, characters 4-11", Belt_MutableSetInt.eq(match$9[0], {
  data: Belt_internalSetInt.fromArray(xs$31)
}));

let xs$32 = Belt_Array.makeBy(500, (x => 1002 + (x << 1) | 0));

b("File \"bs_mutable_set_test.res\", line 279, characters 4-11", Belt_MutableSetInt.eq(match$9[1], {
  data: Belt_internalSetInt.fromArray(xs$32)
}));

let xs$33 = Array_data_util.randomRange(0, 100);

let aa$3 = {
  data: Belt_internalSetInt.fromArray(xs$33)
};

let xs$34 = Array_data_util.randomRange(40, 120);

let bb$3 = {
  data: Belt_internalSetInt.fromArray(xs$34)
};

let cc$2 = Belt_MutableSetInt.union(aa$3, bb$3);

let xs$35 = Array_data_util.randomRange(0, 120);

b("File \"bs_mutable_set_test.res\", line 289, characters 4-11", Belt_MutableSetInt.eq(cc$2, {
  data: Belt_internalSetInt.fromArray(xs$35)
}));

let xs$36 = Array_data_util.randomRange(0, 20);

let xs$37 = Array_data_util.randomRange(21, 40);

let xs$38 = Array_data_util.randomRange(0, 40);

b("File \"bs_mutable_set_test.res\", line 292, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.union({
  data: Belt_internalSetInt.fromArray(xs$36)
}, {
  data: Belt_internalSetInt.fromArray(xs$37)
}), {
  data: Belt_internalSetInt.fromArray(xs$38)
}));

let dd$1 = Belt_MutableSetInt.intersect(aa$3, bb$3);

let xs$39 = Array_data_util.randomRange(40, 100);

b("File \"bs_mutable_set_test.res\", line 296, characters 4-11", Belt_MutableSetInt.eq(dd$1, {
  data: Belt_internalSetInt.fromArray(xs$39)
}));

let xs$40 = Array_data_util.randomRange(0, 20);

let xs$41 = Array_data_util.randomRange(21, 40);

b("File \"bs_mutable_set_test.res\", line 298, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.intersect({
  data: Belt_internalSetInt.fromArray(xs$40)
}, {
  data: Belt_internalSetInt.fromArray(xs$41)
}), {
  data: undefined
}));

let xs$42 = Array_data_util.randomRange(21, 40);

let xs$43 = Array_data_util.randomRange(0, 20);

b("File \"bs_mutable_set_test.res\", line 302, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.intersect({
  data: Belt_internalSetInt.fromArray(xs$42)
}, {
  data: Belt_internalSetInt.fromArray(xs$43)
}), {
  data: undefined
}));

b("File \"bs_mutable_set_test.res\", line 305, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.intersect({
  data: Belt_internalSetInt.fromArray([
    1,
    3,
    4,
    5,
    7,
    9
  ])
}, {
  data: Belt_internalSetInt.fromArray([
    2,
    4,
    5,
    6,
    8,
    10
  ])
}), {
  data: Belt_internalSetInt.fromArray([
    4,
    5
  ])
}));

let xs$44 = Array_data_util.randomRange(0, 39);

b("File \"bs_mutable_set_test.res\", line 306, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff(aa$3, bb$3), {
  data: Belt_internalSetInt.fromArray(xs$44)
}));

let xs$45 = Array_data_util.randomRange(101, 120);

b("File \"bs_mutable_set_test.res\", line 307, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff(bb$3, aa$3), {
  data: Belt_internalSetInt.fromArray(xs$45)
}));

let xs$46 = Array_data_util.randomRange(21, 40);

let xs$47 = Array_data_util.randomRange(0, 20);

let xs$48 = Array_data_util.randomRange(21, 40);

b("File \"bs_mutable_set_test.res\", line 309, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff({
  data: Belt_internalSetInt.fromArray(xs$46)
}, {
  data: Belt_internalSetInt.fromArray(xs$47)
}), {
  data: Belt_internalSetInt.fromArray(xs$48)
}));

let xs$49 = Array_data_util.randomRange(0, 20);

let xs$50 = Array_data_util.randomRange(21, 40);

let xs$51 = Array_data_util.randomRange(0, 20);

b("File \"bs_mutable_set_test.res\", line 316, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff({
  data: Belt_internalSetInt.fromArray(xs$49)
}, {
  data: Belt_internalSetInt.fromArray(xs$50)
}), {
  data: Belt_internalSetInt.fromArray(xs$51)
}));

let xs$52 = Array_data_util.randomRange(0, 20);

let xs$53 = Array_data_util.randomRange(0, 40);

let xs$54 = Array_data_util.randomRange(0, -1);

b("File \"bs_mutable_set_test.res\", line 324, characters 4-11", Belt_MutableSetInt.eq(Belt_MutableSetInt.diff({
  data: Belt_internalSetInt.fromArray(xs$52)
}, {
  data: Belt_internalSetInt.fromArray(xs$53)
}), {
  data: Belt_internalSetInt.fromArray(xs$54)
}));

Mt.from_pair_suites("Bs_mutable_set_test", suites.contents);

let N;

let I;

let R;

let A;

let L;

let empty = Belt_MutableSetInt.make;

let fromArray = Belt_MutableSetInt.fromArray;

let $plus$plus = Belt_MutableSetInt.union;

let f = Belt_MutableSetInt.fromArray;

let $eq$tilde = Belt_MutableSetInt.eq;

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

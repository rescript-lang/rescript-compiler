'use strict';

var Mt = require("./mt.js");
var Bs_Sort = require("../../lib/js/bs_Sort.js");
var Hashtbl = require("../../lib/js/hashtbl.js");
var Bs_Array = require("../../lib/js/bs_Array.js");
var Bs_HashMap = require("../../lib/js/bs_HashMap.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Array_data_util = require("./array_data_util.js");
var Bs_internalBuckets = require("../../lib/js/bs_internalBuckets.js");
var Bs_internalBucketsType = require("../../lib/js/bs_internalBucketsType.js");

var suites = [/* [] */0];

var test_id = [0];

function eqx(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

function eq(x, y) {
  return +(x === y);
}

var hash = Hashtbl.hash;

var cmp = Caml_primitive.caml_int_compare;

var Y = /* module */[
  /* hash */hash,
  /* eq */eq
];

var empty = {
  dict: Y,
  data: Bs_internalBucketsType.create0(30)
};

function add(x, y) {
  return x + y | 0;
}

Bs_HashMap.mergeArrayDone(empty, /* array */[
      /* tuple */[
        1,
        1
      ],
      /* tuple */[
        2,
        3
      ],
      /* tuple */[
        3,
        3
      ],
      /* tuple */[
        2,
        2
      ]
    ]);

eqx("File \"bs_hashmap_test.ml\", line 31, characters 6-13", Bs_HashMap.get(empty, 2), /* Some */[2]);

eqx("File \"bs_hashmap_test.ml\", line 32, characters 6-13", empty.data.size, 3);

var u = Bs_Array.concat(Array_data_util.randomRange(30, 100), Array_data_util.randomRange(40, 120));

var v = Bs_Array.zip(u, u);

var xx = Bs_HashMap.ofArray(v, Y);

eqx("File \"bs_hashmap_test.ml\", line 40, characters 6-13", xx.data.size, 91);

eqx("File \"bs_hashmap_test.ml\", line 41, characters 6-13", Bs_Sort.sortByCont(Bs_internalBuckets.keys0(xx.data), cmp), Array_data_util.range(30, 120));

var u$1 = Bs_Array.concat(Array_data_util.randomRange(0, 100000), Array_data_util.randomRange(0, 100));

var v$1 = {
  dict: Y,
  data: Bs_internalBucketsType.create0(40)
};

Bs_HashMap.mergeArrayDone(v$1, Bs_Array.zip(u$1, u$1));

eqx("File \"bs_hashmap_test.ml\", line 47, characters 6-13", v$1.data.size, 100001);

for(var i = 0; i <= 1000; ++i){
  Bs_HashMap.removeDone(v$1, i);
}

eqx("File \"bs_hashmap_test.ml\", line 51, characters 6-13", v$1.data.size, 99000);

for(var i$1 = 0; i$1 <= 2000; ++i$1){
  Bs_HashMap.removeDone(v$1, i$1);
}

eqx("File \"bs_hashmap_test.ml\", line 55, characters 6-13", v$1.data.size, 98000);

b("File \"bs_hashmap_test.ml\", line 56, characters 4-11", Bs_Array.every(Array_data_util.range(2001, 100000), (function (x) {
            return Bs_HashMap.has(v$1, x);
          })));

Mt.from_pair_suites("bs_hashmap_test.ml", suites[0]);

var N = 0;

var S = 0;

var I = 0;

var $plus$plus = Bs_Array.concat;

var A = 0;

var So = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eqx = eqx;
exports.b = b;
exports.N = N;
exports.S = S;
exports.eq = eq;
exports.hash = hash;
exports.cmp = cmp;
exports.Y = Y;
exports.empty = empty;
exports.I = I;
exports.$plus$plus = $plus$plus;
exports.add = add;
exports.A = A;
exports.So = So;
/* empty Not a pure module */

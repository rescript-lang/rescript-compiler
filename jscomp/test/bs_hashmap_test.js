'use strict';

var Mt = require("./mt.js");
var Belt_Id = require("../../lib/js/belt_Id.js");
var Hashtbl = require("../../lib/js/hashtbl.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Belt_HashMap = require("../../lib/js/belt_HashMap.js");
var Belt_SortArray = require("../../lib/js/belt_SortArray.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Array_data_util = require("./array_data_util.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eqx(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

function eq(x, y) {
  return x === y;
}

var hash = Hashtbl.hash;

var cmp = Caml_primitive.caml_int_compare;

var Y = Belt_Id.hashable(hash, eq);

var empty = Belt_HashMap.make(30, Y);

function add(prim, prim$1) {
  return prim + prim$1 | 0;
}

Belt_HashMap.mergeMany(empty, /* array */[
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

eqx("File \"bs_hashmap_test.ml\", line 31, characters 6-13", Belt_HashMap.get(empty, 2), 2);

eqx("File \"bs_hashmap_test.ml\", line 32, characters 6-13", empty.size, 3);

var u = Belt_Array.concat(Array_data_util.randomRange(30, 100), Array_data_util.randomRange(40, 120));

var v = Belt_Array.zip(u, u);

var xx = Belt_HashMap.fromArray(v, Y);

eqx("File \"bs_hashmap_test.ml\", line 41, characters 6-13", xx.size, 91);

eqx("File \"bs_hashmap_test.ml\", line 42, characters 6-13", Belt_SortArray.stableSortBy(Belt_HashMap.keysToArray(xx), cmp), Array_data_util.range(30, 120));

var u$1 = Belt_Array.concat(Array_data_util.randomRange(0, 100000), Array_data_util.randomRange(0, 100));

var v$1 = Belt_HashMap.make(40, Y);

Belt_HashMap.mergeMany(v$1, Belt_Array.zip(u$1, u$1));

eqx("File \"bs_hashmap_test.ml\", line 48, characters 6-13", v$1.size, 100001);

for(var i = 0; i <= 1000; ++i){
  Belt_HashMap.remove(v$1, i);
}

eqx("File \"bs_hashmap_test.ml\", line 52, characters 6-13", v$1.size, 99000);

for(var i$1 = 0; i$1 <= 2000; ++i$1){
  Belt_HashMap.remove(v$1, i$1);
}

eqx("File \"bs_hashmap_test.ml\", line 56, characters 6-13", v$1.size, 98000);

b("File \"bs_hashmap_test.ml\", line 57, characters 4-11", Belt_Array.every(Array_data_util.range(2001, 100000), (function (x) {
            return Belt_HashMap.has(v$1, x);
          })));

Mt.from_pair_suites("Bs_hashmap_test", suites.contents);

var N = 0;

var S = 0;

var I = 0;

var $plus$plus = Belt_Array.concat;

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
/* Y Not a pure module */

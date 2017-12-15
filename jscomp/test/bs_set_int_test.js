'use strict';

var Mt        = require("./mt.js");
var List      = require("../../lib/js/list.js");
var $$Array   = require("../../lib/js/array.js");
var Block     = require("../../lib/js/block.js");
var Bs_SetInt = require("../../lib/js/bs_SetInt.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

function b(loc, v) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
          return /* Ok */Block.__(4, [v]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

function $eq$tilde(s, i) {
  return Bs_SetInt.eq(Bs_SetInt.ofArray(i), s);
}

function $eq$star(a, b) {
  return Bs_SetInt.eq(Bs_SetInt.ofArray(a), Bs_SetInt.ofArray(b));
}

b("File \"bs_set_int_test.ml\", line 24, characters 4-11", $eq$star(/* int array */[
          1,
          2,
          3
        ], /* int array */[
          3,
          2,
          1
        ]));

var u = Bs_SetInt.inter(Bs_SetInt.ofArray(/* int array */[
          1,
          2,
          3
        ]), Bs_SetInt.ofArray(/* int array */[
          3,
          4,
          5
        ]));

b("File \"bs_set_int_test.ml\", line 30, characters 4-11", Bs_SetInt.eq(Bs_SetInt.ofArray(/* int array */[3]), u));

function range(i, j) {
  return $$Array.init((j - i | 0) + 1 | 0, (function (k) {
                return k + i | 0;
              }));
}

function revRange(i, j) {
  return $$Array.of_list(List.rev($$Array.to_list($$Array.init((j - i | 0) + 1 | 0, (function (k) {
                            return k + i | 0;
                          })))));
}

var v = Bs_SetInt.ofArray($$Array.append(range(100, 1000), revRange(400, 1500)));

var i = range(100, 1500);

b("File \"bs_set_int_test.ml\", line 43, characters 4-11", Bs_SetInt.eq(Bs_SetInt.ofArray(i), v));

var match = Bs_SetInt.partition((function (x) {
        return +(x % 3 === 0);
      }), v);

var l = Bs_SetInt.empty;

var r = Bs_SetInt.empty;

for(var i$1 = 100; i$1 <= 1500; ++i$1){
  if (i$1 % 3) {
    r = Bs_SetInt.add(i$1, r);
  } else {
    l = Bs_SetInt.add(i$1, l);
  }
}

b("File \"bs_set_int_test.ml\", line 54, characters 4-11", Bs_SetInt.eq(match[0], l));

b("File \"bs_set_int_test.ml\", line 55, characters 4-11", Bs_SetInt.eq(match[1], r));

var i$2 = range(50, 100);

var s = Bs_SetInt.inter(Bs_SetInt.ofArray(range(1, 100)), Bs_SetInt.ofArray(range(50, 200)));

b("File \"bs_set_int_test.ml\", line 58, characters 4-11", Bs_SetInt.eq(Bs_SetInt.ofArray(i$2), s));

var i$3 = range(1, 200);

var s$1 = Bs_SetInt.union(Bs_SetInt.ofArray(range(1, 100)), Bs_SetInt.ofArray(range(50, 200)));

b("File \"bs_set_int_test.ml\", line 61, characters 4-11", Bs_SetInt.eq(Bs_SetInt.ofArray(i$3), s$1));

var i$4 = range(1, 49);

var s$2 = Bs_SetInt.diff(Bs_SetInt.ofArray(range(1, 100)), Bs_SetInt.ofArray(range(50, 200)));

b("File \"bs_set_int_test.ml\", line 64, characters 6-13", Bs_SetInt.eq(Bs_SetInt.ofArray(i$4), s$2));

var i$5 = revRange(50, 100);

var s$3 = Bs_SetInt.inter(Bs_SetInt.ofArray(revRange(1, 100)), Bs_SetInt.ofArray(revRange(50, 200)));

b("File \"bs_set_int_test.ml\", line 67, characters 4-11", Bs_SetInt.eq(Bs_SetInt.ofArray(i$5), s$3));

var i$6 = revRange(1, 200);

var s$4 = Bs_SetInt.union(Bs_SetInt.ofArray(revRange(1, 100)), Bs_SetInt.ofArray(revRange(50, 200)));

b("File \"bs_set_int_test.ml\", line 70, characters 4-11", Bs_SetInt.eq(Bs_SetInt.ofArray(i$6), s$4));

var i$7 = revRange(1, 49);

var s$5 = Bs_SetInt.diff(Bs_SetInt.ofArray(revRange(1, 100)), Bs_SetInt.ofArray(revRange(50, 200)));

b("File \"bs_set_int_test.ml\", line 73, characters 6-13", Bs_SetInt.eq(Bs_SetInt.ofArray(i$7), s$5));

var ss = /* array */[
  1,
  222,
  3,
  4,
  2,
  0,
  33,
  -1
];

var v$1 = Bs_SetInt.ofArray(/* array */[
      1,
      222,
      3,
      4,
      2,
      0,
      33,
      -1
    ]);

var minv = Bs_SetInt.min(v$1);

var maxv = Bs_SetInt.max(v$1);

eq("File \"bs_set_int_test.ml\", line 79, characters 5-12", Bs_SetInt.fold((function (x, y) {
            return x + y | 0;
          }), v$1, 0), $$Array.fold_left((function (prim, prim$1) {
            return prim + prim$1 | 0;
          }), 0, ss));

eq("File \"bs_set_int_test.ml\", line 80, characters 5-12", minv, /* Some */[-1]);

eq("File \"bs_set_int_test.ml\", line 81, characters 5-12", maxv, /* Some */[222]);

var v$2 = Bs_SetInt.remove(3, v$1);

var minv$1 = Bs_SetInt.min(v$2);

var maxv$1 = Bs_SetInt.max(v$2);

eq("File \"bs_set_int_test.ml\", line 84, characters 5-12", minv$1, /* Some */[-1]);

eq("File \"bs_set_int_test.ml\", line 85, characters 5-12", maxv$1, /* Some */[222]);

var v$3 = Bs_SetInt.remove(222, v$2);

var minv$2 = Bs_SetInt.min(v$3);

var maxv$2 = Bs_SetInt.max(v$3);

eq("File \"bs_set_int_test.ml\", line 88, characters 5-12", minv$2, /* Some */[-1]);

eq("File \"bs_set_int_test.ml\", line 89, characters 5-12", maxv$2, /* Some */[33]);

var v$4 = Bs_SetInt.remove(-1, v$3);

var minv$3 = Bs_SetInt.min(v$4);

var maxv$3 = Bs_SetInt.max(v$4);

eq("File \"bs_set_int_test.ml\", line 92, characters 5-12", minv$3, /* Some */[0]);

eq("File \"bs_set_int_test.ml\", line 93, characters 5-12", maxv$3, /* Some */[33]);

var v$5 = Bs_SetInt.remove(0, v$4);

var v$6 = Bs_SetInt.remove(33, v$5);

var v$7 = Bs_SetInt.remove(2, v$6);

var v$8 = Bs_SetInt.remove(3, v$7);

var v$9 = Bs_SetInt.remove(4, v$8);

var v$10 = Bs_SetInt.remove(1, v$9);

b("File \"bs_set_int_test.ml\", line 100, characters 4-11", Bs_SetInt.isEmpty(v$10));

Mt.from_pair_suites("bs_set_int_test.ml", suites[0]);

var N = 0;

var ofA = Bs_SetInt.ofArray;

exports.suites    = suites;
exports.test_id   = test_id;
exports.eq        = eq;
exports.b         = b;
exports.N         = N;
exports.$eq$tilde = $eq$tilde;
exports.$eq$star  = $eq$star;
exports.ofA       = ofA;
exports.u         = u;
exports.range     = range;
exports.revRange  = revRange;
/*  Not a pure module */

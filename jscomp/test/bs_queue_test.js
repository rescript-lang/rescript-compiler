'use strict';

var Curry = require("../../lib/js/curry.js");
var Bs_Array = require("../../lib/js/bs_Array.js");
var Bs_Queue = require("../../lib/js/bs_Queue.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function does_raise(f, q) {
  try {
    Curry._1(f, q);
    return /* false */0;
  }
  catch (exn){
    return /* true */1;
  }
}

var q = Bs_Queue.create(/* () */0);

if (!(Caml_obj.caml_equal(Bs_Queue.toArray(q), /* int array */[]) && q.length === 0)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          16,
          2
        ]
      ];
}

if (!(Caml_obj.caml_equal(Bs_Queue.toArray(Bs_Queue.add(q, 1)), /* int array */[1]) && q.length === 1)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          17,
          2
        ]
      ];
}

if (!(Caml_obj.caml_equal(Bs_Queue.toArray(Bs_Queue.add(q, 2)), /* int array */[
          1,
          2
        ]) && q.length === 2)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          18,
          2
        ]
      ];
}

if (!(Caml_obj.caml_equal(Bs_Queue.toArray(Bs_Queue.add(q, 3)), /* int array */[
          1,
          2,
          3
        ]) && q.length === 3)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          19,
          2
        ]
      ];
}

if (!(Caml_obj.caml_equal(Bs_Queue.toArray(Bs_Queue.add(q, 4)), /* int array */[
          1,
          2,
          3,
          4
        ]) && q.length === 4)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          20,
          2
        ]
      ];
}

if (Bs_Queue.popExn(q) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          21,
          2
        ]
      ];
}

if (!(Caml_obj.caml_equal(Bs_Queue.toArray(q), /* int array */[
          2,
          3,
          4
        ]) && q.length === 3)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          21,
          27
        ]
      ];
}

if (Bs_Queue.popExn(q) !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          22,
          2
        ]
      ];
}

if (!(Caml_obj.caml_equal(Bs_Queue.toArray(q), /* int array */[
          3,
          4
        ]) && q.length === 2)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          22,
          27
        ]
      ];
}

if (Bs_Queue.popExn(q) !== 3) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          23,
          2
        ]
      ];
}

if (!(Caml_obj.caml_equal(Bs_Queue.toArray(q), /* int array */[4]) && q.length === 1)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          23,
          27
        ]
      ];
}

if (Bs_Queue.popExn(q) !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          24,
          2
        ]
      ];
}

if (!(Caml_obj.caml_equal(Bs_Queue.toArray(q), /* int array */[]) && q.length === 0)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          24,
          27
        ]
      ];
}

if (!does_raise(Bs_Queue.popExn, q)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          25,
          2
        ]
      ];
}

var q$1 = Bs_Queue.create(/* () */0);

if (Bs_Queue.popExn(Bs_Queue.add(q$1, 1)) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          30,
          2
        ]
      ];
}

if (!does_raise(Bs_Queue.popExn, q$1)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          30,
          34
        ]
      ];
}

if (Bs_Queue.popExn(Bs_Queue.add(q$1, 2)) !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          31,
          2
        ]
      ];
}

if (!does_raise(Bs_Queue.popExn, q$1)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          31,
          34
        ]
      ];
}

if (q$1.length !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          32,
          2
        ]
      ];
}

var q$2 = Bs_Queue.create(/* () */0);

if (Bs_Queue.peekExn(Bs_Queue.add(q$2, 1)) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          37,
          2
        ]
      ];
}

if (Bs_Queue.peekExn(Bs_Queue.add(q$2, 2)) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          38,
          2
        ]
      ];
}

if (Bs_Queue.peekExn(Bs_Queue.add(q$2, 3)) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          39,
          2
        ]
      ];
}

if (Bs_Queue.peekExn(q$2) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          40,
          2
        ]
      ];
}

if (Bs_Queue.popExn(q$2) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          40,
          28
        ]
      ];
}

if (Bs_Queue.peekExn(q$2) !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          41,
          2
        ]
      ];
}

if (Bs_Queue.popExn(q$2) !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          41,
          28
        ]
      ];
}

if (Bs_Queue.peekExn(q$2) !== 3) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          42,
          2
        ]
      ];
}

if (Bs_Queue.popExn(q$2) !== 3) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          42,
          28
        ]
      ];
}

if (!does_raise(Bs_Queue.peekExn, q$2)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          43,
          2
        ]
      ];
}

if (!does_raise(Bs_Queue.peekExn, q$2)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          44,
          2
        ]
      ];
}

var q$3 = Bs_Queue.create(/* () */0);

for(var i = 1; i <= 10; ++i){
  Bs_Queue.addDone(q$3, i);
}

Bs_Queue.clear(q$3);

if (q$3.length !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          51,
          2
        ]
      ];
}

if (!does_raise(Bs_Queue.popExn, q$3)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          52,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(q$3, Bs_Queue.create(/* () */0))) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          53,
          2
        ]
      ];
}

Bs_Queue.addDone(q$3, 42);

if (Bs_Queue.popExn(q$3) !== 42) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          55,
          2
        ]
      ];
}

var q1 = Bs_Queue.create(/* () */0);

for(var i$1 = 1; i$1 <= 10; ++i$1){
  Bs_Queue.addDone(q1, i$1);
}

var q2 = Bs_Queue.copy(q1);

if (!Caml_obj.caml_equal(Bs_Queue.toArray(q1), /* array */[
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10
      ])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          62,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(Bs_Queue.toArray(q2), /* array */[
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10
      ])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          63,
          2
        ]
      ];
}

if (q1.length !== 10) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          64,
          2
        ]
      ];
}

if (q2.length !== 10) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          65,
          2
        ]
      ];
}

for(var i$2 = 1; i$2 <= 10; ++i$2){
  if (Bs_Queue.popExn(q1) !== i$2) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_queue_test.ml",
            67,
            4
          ]
        ];
  }
  
}

for(var i$3 = 1; i$3 <= 10; ++i$3){
  if (Bs_Queue.popExn(q2) !== i$3) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_queue_test.ml",
            70,
            4
          ]
        ];
  }
  
}

var q$4 = Bs_Queue.create(/* () */0);

if (q$4.length !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          76,
          2
        ]
      ];
}

for(var i$4 = 1; i$4 <= 10; ++i$4){
  Bs_Queue.addDone(q$4, i$4);
  if (q$4.length !== i$4) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_queue_test.ml",
            79,
            4
          ]
        ];
  }
  if (!q$4.length) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_queue_test.ml",
            80,
            4
          ]
        ];
  }
  
}

for(var i$5 = 10; i$5 >= 1; --i$5){
  if (q$4.length !== i$5) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_queue_test.ml",
            83,
            4
          ]
        ];
  }
  if (!q$4.length) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_queue_test.ml",
            84,
            4
          ]
        ];
  }
  Bs_Queue.popExn(q$4);
}

if (q$4.length !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          87,
          2
        ]
      ];
}

if (q$4.length !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          88,
          2
        ]
      ];
}

var q$5 = Bs_Queue.create(/* () */0);

for(var i$6 = 1; i$6 <= 10; ++i$6){
  Bs_Queue.addDone(q$5, i$6);
}

var i$7 = [1];

Bs_Queue.forEach(q$5, (function (j) {
        if (i$7[0] !== j) {
          throw [
                Caml_builtin_exceptions.assert_failure,
                [
                  "bs_queue_test.ml",
                  95,
                  29
                ]
              ];
        }
        i$7[0] = i$7[0] + 1 | 0;
        return /* () */0;
      }));

var q1$1 = Bs_Queue.create(/* () */0);

var q2$1 = Bs_Queue.create(/* () */0);

if (q1$1.length !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          100,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(Bs_Queue.toArray(q1$1), /* array */[])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          100,
          28
        ]
      ];
}

if (q2$1.length !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          101,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(Bs_Queue.toArray(q2$1), /* array */[])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          101,
          28
        ]
      ];
}

Bs_Queue.transfer(q1$1, q2$1);

if (q1$1.length !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          103,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(Bs_Queue.toArray(q1$1), /* array */[])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          103,
          28
        ]
      ];
}

if (q2$1.length !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          104,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(Bs_Queue.toArray(q2$1), /* array */[])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          104,
          28
        ]
      ];
}

var q1$2 = Bs_Queue.create(/* () */0);

var q2$2 = Bs_Queue.create(/* () */0);

for(var i$8 = 1; i$8 <= 4; ++i$8){
  Bs_Queue.addDone(q1$2, i$8);
}

if (q1$2.length !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          110,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(Bs_Queue.toArray(q1$2), /* int array */[
        1,
        2,
        3,
        4
      ])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          110,
          28
        ]
      ];
}

if (q2$2.length !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          111,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(Bs_Queue.toArray(q2$2), /* int array */[])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          111,
          28
        ]
      ];
}

Bs_Queue.transfer(q1$2, q2$2);

if (q1$2.length !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          113,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(Bs_Queue.toArray(q1$2), /* int array */[])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          113,
          28
        ]
      ];
}

if (q2$2.length !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          114,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(Bs_Queue.toArray(q2$2), /* int array */[
        1,
        2,
        3,
        4
      ])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          114,
          28
        ]
      ];
}

var q1$3 = Bs_Queue.create(/* () */0);

var q2$3 = Bs_Queue.create(/* () */0);

for(var i$9 = 5; i$9 <= 8; ++i$9){
  Bs_Queue.addDone(q2$3, i$9);
}

if (q1$3.length !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          120,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(Bs_Queue.toArray(q1$3), /* int array */[])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          120,
          28
        ]
      ];
}

if (q2$3.length !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          121,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(Bs_Queue.toArray(q2$3), /* int array */[
        5,
        6,
        7,
        8
      ])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          121,
          28
        ]
      ];
}

Bs_Queue.transfer(q1$3, q2$3);

if (q1$3.length !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          123,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(Bs_Queue.toArray(q1$3), /* int array */[])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          123,
          28
        ]
      ];
}

if (q2$3.length !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          124,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(Bs_Queue.toArray(q2$3), /* int array */[
        5,
        6,
        7,
        8
      ])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          124,
          28
        ]
      ];
}

var q1$4 = Bs_Queue.create(/* () */0);

var q2$4 = Bs_Queue.create(/* () */0);

for(var i$10 = 1; i$10 <= 4; ++i$10){
  Bs_Queue.addDone(q1$4, i$10);
}

for(var i$11 = 5; i$11 <= 8; ++i$11){
  Bs_Queue.addDone(q2$4, i$11);
}

if (q1$4.length !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          131,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(Bs_Queue.toArray(q1$4), /* int array */[
        1,
        2,
        3,
        4
      ])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          131,
          28
        ]
      ];
}

if (q2$4.length !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          132,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(Bs_Queue.toArray(q2$4), /* int array */[
        5,
        6,
        7,
        8
      ])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          132,
          28
        ]
      ];
}

Bs_Queue.transfer(q1$4, q2$4);

if (q1$4.length !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          134,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(Bs_Queue.toArray(q1$4), /* int array */[])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          134,
          28
        ]
      ];
}

var v = /* array */[
  5,
  6,
  7,
  8,
  1,
  2,
  3,
  4
];

if (q2$4.length !== 8) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          136,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(Bs_Queue.toArray(q2$4), v)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          136,
          28
        ]
      ];
}

if (Bs_Queue.reduce(q2$4, 0, (function (x, y) {
          return x - y | 0;
        })) !== Bs_Array.foldLeft(v, 0, (function (x, y) {
          return x - y | 0;
        }))) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_queue_test.ml",
          138,
          2
        ]
      ];
}

console.log("OK");

var Q = 0;

var $plus$plus = Bs_Queue.add;

exports.Q = Q;
exports.does_raise = does_raise;
exports.$plus$plus = $plus$plus;
/* q Not a pure module */

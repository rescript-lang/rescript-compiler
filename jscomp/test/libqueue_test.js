// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Caml_obj                = require("../runtime/caml_obj");
var Queue                   = require("../stdlib/queue");
var Caml_curry              = require("../runtime/caml_curry");
var List                    = require("../stdlib/list");

var Empty = Queue.Empty;

var create = Queue.create;

var add = Queue.add;

var take = Queue.take;

var peek = Queue.peek;

var clear = Queue.clear;

var copy = Queue.copy;

var is_empty = Queue.is_empty;

var length = Queue.length;

var iter = Queue.iter;

var fold = Queue.fold;

var transfer = Queue.transfer;

function to_list(q) {
  return List.rev(Caml_curry.app3(fold, function (l, x) {
                  return /* :: */[
                          x,
                          l
                        ];
                }, /* [] */0, q));
}

var Q = /* module */[
  Empty,
  create,
  add,
  Queue.push,
  take,
  Queue.pop,
  peek,
  Queue.top,
  clear,
  copy,
  is_empty,
  length,
  iter,
  fold,
  transfer,
  to_list
];

function does_raise(f, q) {
  try {
    Caml_curry.app1(f, q);
    return /* false */0;
  }
  catch (exn){
    if (exn === Empty) {
      return /* true */1;
    }
    else {
      throw exn;
    }
  }
}

var q = Caml_curry.app1(create, /* () */0);

if (!(to_list(q) === /* [] */0 && Caml_curry.app1(length, q) === 0)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          28,
          25
        ]
      ];
}

Caml_curry.app2(add, 1, q);

if (!(Caml_obj.caml_equal(to_list(q), /* :: */[
          1,
          /* [] */0
        ]) && Caml_curry.app1(length, q) === 1)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          29,
          25
        ]
      ];
}

Caml_curry.app2(add, 2, q);

if (!(Caml_obj.caml_equal(to_list(q), /* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ]) && Caml_curry.app1(length, q) === 2)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          30,
          25
        ]
      ];
}

Caml_curry.app2(add, 3, q);

if (!(Caml_obj.caml_equal(to_list(q), /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ]) && Caml_curry.app1(length, q) === 3)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          31,
          25
        ]
      ];
}

Caml_curry.app2(add, 4, q);

if (!(Caml_obj.caml_equal(to_list(q), /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* :: */[
                4,
                /* [] */0
              ]
            ]
          ]
        ]) && Caml_curry.app1(length, q) === 4)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          32,
          25
        ]
      ];
}

if (Caml_curry.app1(take, q) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          33,
          2
        ]
      ];
}

if (!(Caml_obj.caml_equal(to_list(q), /* :: */[
          2,
          /* :: */[
            3,
            /* :: */[
              4,
              /* [] */0
            ]
          ]
        ]) && Caml_curry.app1(length, q) === 3)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          33,
          25
        ]
      ];
}

if (Caml_curry.app1(take, q) !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          34,
          2
        ]
      ];
}

if (!(Caml_obj.caml_equal(to_list(q), /* :: */[
          3,
          /* :: */[
            4,
            /* [] */0
          ]
        ]) && Caml_curry.app1(length, q) === 2)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          34,
          25
        ]
      ];
}

if (Caml_curry.app1(take, q) !== 3) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          35,
          2
        ]
      ];
}

if (!(Caml_obj.caml_equal(to_list(q), /* :: */[
          4,
          /* [] */0
        ]) && Caml_curry.app1(length, q) === 1)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          35,
          25
        ]
      ];
}

if (Caml_curry.app1(take, q) !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          36,
          2
        ]
      ];
}

if (!(to_list(q) === /* [] */0 && Caml_curry.app1(length, q) === 0)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          36,
          25
        ]
      ];
}

if (!does_raise(take, q)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          37,
          2
        ]
      ];
}

var q$1 = Caml_curry.app1(create, /* () */0);

Caml_curry.app2(add, 1, q$1);

if (Caml_curry.app1(take, q$1) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          42,
          13
        ]
      ];
}

if (!does_raise(take, q$1)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          42,
          36
        ]
      ];
}

Caml_curry.app2(add, 2, q$1);

if (Caml_curry.app1(take, q$1) !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          43,
          13
        ]
      ];
}

if (!does_raise(take, q$1)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          43,
          36
        ]
      ];
}

if (Caml_curry.app1(length, q$1) !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          44,
          2
        ]
      ];
}

var q$2 = Caml_curry.app1(create, /* () */0);

Caml_curry.app2(add, 1, q$2);

if (Caml_curry.app1(peek, q$2) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          49,
          13
        ]
      ];
}

Caml_curry.app2(add, 2, q$2);

if (Caml_curry.app1(peek, q$2) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          50,
          13
        ]
      ];
}

Caml_curry.app2(add, 3, q$2);

if (Caml_curry.app1(peek, q$2) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          51,
          13
        ]
      ];
}

if (Caml_curry.app1(peek, q$2) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          52,
          2
        ]
      ];
}

if (Caml_curry.app1(take, q$2) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          52,
          25
        ]
      ];
}

if (Caml_curry.app1(peek, q$2) !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          53,
          2
        ]
      ];
}

if (Caml_curry.app1(take, q$2) !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          53,
          25
        ]
      ];
}

if (Caml_curry.app1(peek, q$2) !== 3) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          54,
          2
        ]
      ];
}

if (Caml_curry.app1(take, q$2) !== 3) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          54,
          25
        ]
      ];
}

if (!does_raise(peek, q$2)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          55,
          2
        ]
      ];
}

if (!does_raise(peek, q$2)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          56,
          2
        ]
      ];
}

var q$3 = Caml_curry.app1(create, /* () */0);

for(var i = 1; i<= 10; ++i){
  Caml_curry.app2(add, i, q$3);
}

Caml_curry.app1(clear, q$3);

if (Caml_curry.app1(length, q$3) !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          63,
          2
        ]
      ];
}

if (!does_raise(take, q$3)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          64,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(q$3, Caml_curry.app1(create, /* () */0))) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          65,
          2
        ]
      ];
}

Caml_curry.app2(add, 42, q$3);

if (Caml_curry.app1(take, q$3) !== 42) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          67,
          2
        ]
      ];
}

var q1 = Caml_curry.app1(create, /* () */0);

for(var i$1 = 1; i$1<= 10; ++i$1){
  Caml_curry.app2(add, i$1, q1);
}

var q2 = Caml_curry.app1(copy, q1);

if (!Caml_obj.caml_equal(to_list(q1), /* :: */[
        1,
        /* :: */[
          2,
          /* :: */[
            3,
            /* :: */[
              4,
              /* :: */[
                5,
                /* :: */[
                  6,
                  /* :: */[
                    7,
                    /* :: */[
                      8,
                      /* :: */[
                        9,
                        /* :: */[
                          10,
                          /* [] */0
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          74,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(to_list(q2), /* :: */[
        1,
        /* :: */[
          2,
          /* :: */[
            3,
            /* :: */[
              4,
              /* :: */[
                5,
                /* :: */[
                  6,
                  /* :: */[
                    7,
                    /* :: */[
                      8,
                      /* :: */[
                        9,
                        /* :: */[
                          10,
                          /* [] */0
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          75,
          2
        ]
      ];
}

if (Caml_curry.app1(length, q1) !== 10) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          76,
          2
        ]
      ];
}

if (Caml_curry.app1(length, q2) !== 10) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          77,
          2
        ]
      ];
}

for(var i$2 = 1; i$2<= 10; ++i$2){
  if (Caml_curry.app1(take, q1) !== i$2) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "libqueue_test.ml",
            79,
            4
          ]
        ];
  }
  
}

for(var i$3 = 1; i$3<= 10; ++i$3){
  if (Caml_curry.app1(take, q2) !== i$3) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "libqueue_test.ml",
            82,
            4
          ]
        ];
  }
  
}

var q$4 = Caml_curry.app1(create, /* () */0);

if (!Caml_curry.app1(is_empty, q$4)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          88,
          2
        ]
      ];
}

for(var i$4 = 1; i$4<= 10; ++i$4){
  Caml_curry.app2(add, i$4, q$4);
  if (Caml_curry.app1(length, q$4) !== i$4) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "libqueue_test.ml",
            91,
            4
          ]
        ];
  }
  if (Caml_curry.app1(is_empty, q$4)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "libqueue_test.ml",
            92,
            4
          ]
        ];
  }
  
}

for(var i$5 = 10; i$5>= 1; --i$5){
  if (Caml_curry.app1(length, q$4) !== i$5) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "libqueue_test.ml",
            95,
            4
          ]
        ];
  }
  if (Caml_curry.app1(is_empty, q$4)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "libqueue_test.ml",
            96,
            4
          ]
        ];
  }
  Caml_curry.app1(take, q$4);
}

if (Caml_curry.app1(length, q$4) !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          99,
          2
        ]
      ];
}

if (!Caml_curry.app1(is_empty, q$4)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          100,
          2
        ]
      ];
}

var q$5 = Caml_curry.app1(create, /* () */0);

for(var i$6 = 1; i$6<= 10; ++i$6){
  Caml_curry.app2(add, i$6, q$5);
}

var i$7 = [1];

Caml_curry.app2(iter, function (j) {
      if (i$7[0] !== j) {
        throw [
              Caml_builtin_exceptions.assert_failure,
              [
                "libqueue_test.ml",
                107,
                19
              ]
            ];
      }
      return ++ i$7[0];
    }, q$5);

var q1$1 = Caml_curry.app1(create, /* () */0);

var q2$1 = Caml_curry.app1(create, /* () */0);

if (Caml_curry.app1(length, q1$1) !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          112,
          2
        ]
      ];
}

if (to_list(q1$1) !== /* [] */0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          112,
          28
        ]
      ];
}

if (Caml_curry.app1(length, q2$1) !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          113,
          2
        ]
      ];
}

if (to_list(q2$1) !== /* [] */0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          113,
          28
        ]
      ];
}

Caml_curry.app2(transfer, q1$1, q2$1);

if (Caml_curry.app1(length, q1$1) !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          115,
          2
        ]
      ];
}

if (to_list(q1$1) !== /* [] */0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          115,
          28
        ]
      ];
}

if (Caml_curry.app1(length, q2$1) !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          116,
          2
        ]
      ];
}

if (to_list(q2$1) !== /* [] */0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          116,
          28
        ]
      ];
}

var q1$2 = Caml_curry.app1(create, /* () */0);

var q2$2 = Caml_curry.app1(create, /* () */0);

for(var i$8 = 1; i$8<= 4; ++i$8){
  Caml_curry.app2(add, i$8, q1$2);
}

if (Caml_curry.app1(length, q1$2) !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          122,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(to_list(q1$2), /* :: */[
        1,
        /* :: */[
          2,
          /* :: */[
            3,
            /* :: */[
              4,
              /* [] */0
            ]
          ]
        ]
      ])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          122,
          28
        ]
      ];
}

if (Caml_curry.app1(length, q2$2) !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          123,
          2
        ]
      ];
}

if (to_list(q2$2) !== /* [] */0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          123,
          28
        ]
      ];
}

Caml_curry.app2(transfer, q1$2, q2$2);

if (Caml_curry.app1(length, q1$2) !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          125,
          2
        ]
      ];
}

if (to_list(q1$2) !== /* [] */0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          125,
          28
        ]
      ];
}

if (Caml_curry.app1(length, q2$2) !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          126,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(to_list(q2$2), /* :: */[
        1,
        /* :: */[
          2,
          /* :: */[
            3,
            /* :: */[
              4,
              /* [] */0
            ]
          ]
        ]
      ])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          126,
          28
        ]
      ];
}

var q1$3 = Caml_curry.app1(create, /* () */0);

var q2$3 = Caml_curry.app1(create, /* () */0);

for(var i$9 = 5; i$9<= 8; ++i$9){
  Caml_curry.app2(add, i$9, q2$3);
}

if (Caml_curry.app1(length, q1$3) !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          132,
          2
        ]
      ];
}

if (to_list(q1$3) !== /* [] */0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          132,
          28
        ]
      ];
}

if (Caml_curry.app1(length, q2$3) !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          133,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(to_list(q2$3), /* :: */[
        5,
        /* :: */[
          6,
          /* :: */[
            7,
            /* :: */[
              8,
              /* [] */0
            ]
          ]
        ]
      ])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          133,
          28
        ]
      ];
}

Caml_curry.app2(transfer, q1$3, q2$3);

if (Caml_curry.app1(length, q1$3) !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          135,
          2
        ]
      ];
}

if (to_list(q1$3) !== /* [] */0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          135,
          28
        ]
      ];
}

if (Caml_curry.app1(length, q2$3) !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          136,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(to_list(q2$3), /* :: */[
        5,
        /* :: */[
          6,
          /* :: */[
            7,
            /* :: */[
              8,
              /* [] */0
            ]
          ]
        ]
      ])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          136,
          28
        ]
      ];
}

var q1$4 = Caml_curry.app1(create, /* () */0);

var q2$4 = Caml_curry.app1(create, /* () */0);

for(var i$10 = 1; i$10<= 4; ++i$10){
  Caml_curry.app2(add, i$10, q1$4);
}

for(var i$11 = 5; i$11<= 8; ++i$11){
  Caml_curry.app2(add, i$11, q2$4);
}

if (Caml_curry.app1(length, q1$4) !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          143,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(to_list(q1$4), /* :: */[
        1,
        /* :: */[
          2,
          /* :: */[
            3,
            /* :: */[
              4,
              /* [] */0
            ]
          ]
        ]
      ])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          143,
          28
        ]
      ];
}

if (Caml_curry.app1(length, q2$4) !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          144,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(to_list(q2$4), /* :: */[
        5,
        /* :: */[
          6,
          /* :: */[
            7,
            /* :: */[
              8,
              /* [] */0
            ]
          ]
        ]
      ])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          144,
          28
        ]
      ];
}

Caml_curry.app2(transfer, q1$4, q2$4);

if (Caml_curry.app1(length, q1$4) !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          146,
          2
        ]
      ];
}

if (to_list(q1$4) !== /* [] */0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          146,
          28
        ]
      ];
}

if (Caml_curry.app1(length, q2$4) !== 8) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          147,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(to_list(q2$4), /* :: */[
        5,
        /* :: */[
          6,
          /* :: */[
            7,
            /* :: */[
              8,
              /* :: */[
                1,
                /* :: */[
                  2,
                  /* :: */[
                    3,
                    /* :: */[
                      4,
                      /* [] */0
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "libqueue_test.ml",
          147,
          28
        ]
      ];
}

console.log("OK");

exports.Q          = Q;
exports.does_raise = does_raise;
/* q Not a pure module */

'use strict';

var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var Queue = require("../../lib/js/queue.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function to_list(q) {
  return List.rev(Queue.fold((function (l, x) {
                    return /* :: */[
                            x,
                            l
                          ];
                  }), /* [] */0, q));
}

var Q = /* module */[
  /* Empty */Queue.Empty,
  /* create */Queue.create,
  /* add */Queue.add,
  /* push */Queue.push,
  /* take */Queue.take,
  /* pop */Queue.pop,
  /* peek */Queue.peek,
  /* top */Queue.top,
  /* clear */Queue.clear,
  /* copy */Queue.copy,
  /* is_empty */Queue.is_empty,
  /* length */Queue.length,
  /* iter */Queue.iter,
  /* fold */Queue.fold,
  /* transfer */Queue.transfer,
  /* to_list */to_list
];

function does_raise(f, q) {
  try {
    Curry._1(f, q);
    return false;
  }
  catch (exn){
    if (exn === Queue.Empty) {
      return true;
    } else {
      throw exn;
    }
  }
}

var q = /* record */[
  /* length */0,
  /* first : Nil */0,
  /* last : Nil */0
];

if (!(to_list(q) === /* [] */0 && q[/* length */0] === 0)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          28,
          25
        ]
      ];
}

Queue.add(1, q);

if (!(Caml_obj.caml_equal(to_list(q), /* :: */[
          1,
          /* [] */0
        ]) && q[/* length */0] === 1)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          29,
          25
        ]
      ];
}

Queue.add(2, q);

if (!(Caml_obj.caml_equal(to_list(q), /* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ]) && q[/* length */0] === 2)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          30,
          25
        ]
      ];
}

Queue.add(3, q);

if (!(Caml_obj.caml_equal(to_list(q), /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ]) && q[/* length */0] === 3)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          31,
          25
        ]
      ];
}

Queue.add(4, q);

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
        ]) && q[/* length */0] === 4)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          32,
          25
        ]
      ];
}

if (Queue.take(q) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
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
        ]) && q[/* length */0] === 3)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          33,
          25
        ]
      ];
}

if (Queue.take(q) !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
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
        ]) && q[/* length */0] === 2)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          34,
          25
        ]
      ];
}

if (Queue.take(q) !== 3) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          35,
          2
        ]
      ];
}

if (!(Caml_obj.caml_equal(to_list(q), /* :: */[
          4,
          /* [] */0
        ]) && q[/* length */0] === 1)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          35,
          25
        ]
      ];
}

if (Queue.take(q) !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          36,
          2
        ]
      ];
}

if (!(to_list(q) === /* [] */0 && q[/* length */0] === 0)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          36,
          25
        ]
      ];
}

if (!does_raise(Queue.take, q)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          37,
          2
        ]
      ];
}

var q$1 = /* record */[
  /* length */0,
  /* first : Nil */0,
  /* last : Nil */0
];

Queue.add(1, q$1);

if (Queue.take(q$1) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          42,
          13
        ]
      ];
}

if (!does_raise(Queue.take, q$1)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          42,
          36
        ]
      ];
}

Queue.add(2, q$1);

if (Queue.take(q$1) !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          43,
          13
        ]
      ];
}

if (!does_raise(Queue.take, q$1)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          43,
          36
        ]
      ];
}

if (q$1[/* length */0] !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          44,
          2
        ]
      ];
}

var q$2 = /* record */[
  /* length */0,
  /* first : Nil */0,
  /* last : Nil */0
];

Queue.add(1, q$2);

if (Queue.peek(q$2) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          49,
          13
        ]
      ];
}

Queue.add(2, q$2);

if (Queue.peek(q$2) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          50,
          13
        ]
      ];
}

Queue.add(3, q$2);

if (Queue.peek(q$2) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          51,
          13
        ]
      ];
}

if (Queue.peek(q$2) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          52,
          2
        ]
      ];
}

if (Queue.take(q$2) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          52,
          25
        ]
      ];
}

if (Queue.peek(q$2) !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          53,
          2
        ]
      ];
}

if (Queue.take(q$2) !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          53,
          25
        ]
      ];
}

if (Queue.peek(q$2) !== 3) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          54,
          2
        ]
      ];
}

if (Queue.take(q$2) !== 3) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          54,
          25
        ]
      ];
}

if (!does_raise(Queue.peek, q$2)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          55,
          2
        ]
      ];
}

if (!does_raise(Queue.peek, q$2)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          56,
          2
        ]
      ];
}

var q$3 = /* record */[
  /* length */0,
  /* first : Nil */0,
  /* last : Nil */0
];

for(var i = 1; i <= 10; ++i){
  Queue.add(i, q$3);
}

Queue.clear(q$3);

if (q$3[/* length */0] !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          63,
          2
        ]
      ];
}

if (!does_raise(Queue.take, q$3)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          64,
          2
        ]
      ];
}

if (!Caml_obj.caml_equal(q$3, /* record */[
        /* length */0,
        /* first : Nil */0,
        /* last : Nil */0
      ])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          65,
          2
        ]
      ];
}

Queue.add(42, q$3);

if (Queue.take(q$3) !== 42) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          67,
          2
        ]
      ];
}

var q1 = /* record */[
  /* length */0,
  /* first : Nil */0,
  /* last : Nil */0
];

for(var i$1 = 1; i$1 <= 10; ++i$1){
  Queue.add(i$1, q1);
}

var q2 = Queue.copy(q1);

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
        /* tuple */[
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
        /* tuple */[
          "libqueue_test.ml",
          75,
          2
        ]
      ];
}

if (q1[/* length */0] !== 10) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          76,
          2
        ]
      ];
}

if (q2[/* length */0] !== 10) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          77,
          2
        ]
      ];
}

for(var i$2 = 1; i$2 <= 10; ++i$2){
  if (Queue.take(q1) !== i$2) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "libqueue_test.ml",
            79,
            4
          ]
        ];
  }
  
}

for(var i$3 = 1; i$3 <= 10; ++i$3){
  if (Queue.take(q2) !== i$3) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "libqueue_test.ml",
            82,
            4
          ]
        ];
  }
  
}

var q$4 = /* record */[
  /* length */0,
  /* first : Nil */0,
  /* last : Nil */0
];

if (q$4[/* length */0] !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          88,
          2
        ]
      ];
}

for(var i$4 = 1; i$4 <= 10; ++i$4){
  Queue.add(i$4, q$4);
  if (q$4[/* length */0] !== i$4) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "libqueue_test.ml",
            91,
            4
          ]
        ];
  }
  if (q$4[/* length */0] === 0) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "libqueue_test.ml",
            92,
            4
          ]
        ];
  }
  
}

for(var i$5 = 10; i$5 >= 1; --i$5){
  if (q$4[/* length */0] !== i$5) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "libqueue_test.ml",
            95,
            4
          ]
        ];
  }
  if (q$4[/* length */0] === 0) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "libqueue_test.ml",
            96,
            4
          ]
        ];
  }
  Queue.take(q$4);
}

if (q$4[/* length */0] !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          99,
          2
        ]
      ];
}

if (q$4[/* length */0] !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          100,
          2
        ]
      ];
}

var q$5 = /* record */[
  /* length */0,
  /* first : Nil */0,
  /* last : Nil */0
];

for(var i$6 = 1; i$6 <= 10; ++i$6){
  Queue.add(i$6, q$5);
}

var i$7 = /* record */[/* contents */1];

Queue.iter((function (j) {
        if (i$7[0] !== j) {
          throw [
                Caml_builtin_exceptions.assert_failure,
                /* tuple */[
                  "libqueue_test.ml",
                  107,
                  19
                ]
              ];
        }
        i$7[0] = i$7[0] + 1 | 0;
        return /* () */0;
      }), q$5);

var q1$1 = /* record */[
  /* length */0,
  /* first : Nil */0,
  /* last : Nil */0
];

var q2$1 = /* record */[
  /* length */0,
  /* first : Nil */0,
  /* last : Nil */0
];

if (q1$1[/* length */0] !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          112,
          2
        ]
      ];
}

if (to_list(q1$1) !== /* [] */0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          112,
          28
        ]
      ];
}

if (q2$1[/* length */0] !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          113,
          2
        ]
      ];
}

if (to_list(q2$1) !== /* [] */0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          113,
          28
        ]
      ];
}

Queue.transfer(q1$1, q2$1);

if (q1$1[/* length */0] !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          115,
          2
        ]
      ];
}

if (to_list(q1$1) !== /* [] */0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          115,
          28
        ]
      ];
}

if (q2$1[/* length */0] !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          116,
          2
        ]
      ];
}

if (to_list(q2$1) !== /* [] */0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          116,
          28
        ]
      ];
}

var q1$2 = /* record */[
  /* length */0,
  /* first : Nil */0,
  /* last : Nil */0
];

var q2$2 = /* record */[
  /* length */0,
  /* first : Nil */0,
  /* last : Nil */0
];

for(var i$8 = 1; i$8 <= 4; ++i$8){
  Queue.add(i$8, q1$2);
}

if (q1$2[/* length */0] !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
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
        /* tuple */[
          "libqueue_test.ml",
          122,
          28
        ]
      ];
}

if (q2$2[/* length */0] !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          123,
          2
        ]
      ];
}

if (to_list(q2$2) !== /* [] */0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          123,
          28
        ]
      ];
}

Queue.transfer(q1$2, q2$2);

if (q1$2[/* length */0] !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          125,
          2
        ]
      ];
}

if (to_list(q1$2) !== /* [] */0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          125,
          28
        ]
      ];
}

if (q2$2[/* length */0] !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
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
        /* tuple */[
          "libqueue_test.ml",
          126,
          28
        ]
      ];
}

var q1$3 = /* record */[
  /* length */0,
  /* first : Nil */0,
  /* last : Nil */0
];

var q2$3 = /* record */[
  /* length */0,
  /* first : Nil */0,
  /* last : Nil */0
];

for(var i$9 = 5; i$9 <= 8; ++i$9){
  Queue.add(i$9, q2$3);
}

if (q1$3[/* length */0] !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          132,
          2
        ]
      ];
}

if (to_list(q1$3) !== /* [] */0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          132,
          28
        ]
      ];
}

if (q2$3[/* length */0] !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
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
        /* tuple */[
          "libqueue_test.ml",
          133,
          28
        ]
      ];
}

Queue.transfer(q1$3, q2$3);

if (q1$3[/* length */0] !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          135,
          2
        ]
      ];
}

if (to_list(q1$3) !== /* [] */0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          135,
          28
        ]
      ];
}

if (q2$3[/* length */0] !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
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
        /* tuple */[
          "libqueue_test.ml",
          136,
          28
        ]
      ];
}

var q1$4 = /* record */[
  /* length */0,
  /* first : Nil */0,
  /* last : Nil */0
];

var q2$4 = /* record */[
  /* length */0,
  /* first : Nil */0,
  /* last : Nil */0
];

for(var i$10 = 1; i$10 <= 4; ++i$10){
  Queue.add(i$10, q1$4);
}

for(var i$11 = 5; i$11 <= 8; ++i$11){
  Queue.add(i$11, q2$4);
}

if (q1$4[/* length */0] !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
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
        /* tuple */[
          "libqueue_test.ml",
          143,
          28
        ]
      ];
}

if (q2$4[/* length */0] !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
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
        /* tuple */[
          "libqueue_test.ml",
          144,
          28
        ]
      ];
}

Queue.transfer(q1$4, q2$4);

if (q1$4[/* length */0] !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          146,
          2
        ]
      ];
}

if (to_list(q1$4) !== /* [] */0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "libqueue_test.ml",
          146,
          28
        ]
      ];
}

if (q2$4[/* length */0] !== 8) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
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
        /* tuple */[
          "libqueue_test.ml",
          147,
          28
        ]
      ];
}

console.log("OK");

exports.Q = Q;
exports.does_raise = does_raise;
/* q Not a pure module */

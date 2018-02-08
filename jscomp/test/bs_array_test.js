'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_list = require("../../lib/js/js_list.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Belt_List = require("../../lib/js/belt_List.js");
var Js_vector = require("../../lib/js/js_vector.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Caml_array = require("../../lib/js/caml_array.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

function neq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
          return /* Neq */Block.__(1, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

console.log(/* int array */[
            1,
            2,
            3,
            4
          ].filter((function (x) {
                return +(x > 2);
              })).map((function (x, i) {
              return x + i | 0;
            })).reduce((function (x, y) {
            return x + y | 0;
          }), 0));

function id(x) {
  return eq("File \"bs_array_test.ml\", line 24, characters 5-12", Js_vector.toList(Js_list.toVector(x)), x);
}

eq("File \"bs_array_test.ml\", line 28, characters 5-12", Js_list.toVector(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ]), /* int array */[
      1,
      2,
      3
    ]);

eq("File \"bs_array_test.ml\", line 29, characters 6-13", Js_vector.map((function (x) {
            return x + 1 | 0;
          }), /* int array */[
          1,
          2,
          3
        ]), /* int array */[
      2,
      3,
      4
    ]);

eq("File \"bs_array_test.ml\", line 32, characters 5-12", Caml_array.caml_make_vect(5, 3), /* array */[
      3,
      3,
      3,
      3,
      3
    ]);

var a = Js_vector.init(5, (function (i) {
        return i + 1 | 0;
      }));

eq("File \"bs_array_test.ml\", line 34, characters 5-12", (Js_vector.filterInPlace((function (j) {
              return +(j % 2 === 0);
            }), a), a), /* int array */[
      2,
      4
    ]);

var a$1 = Js_vector.init(5, (function (i) {
        return i + 1 | 0;
      }));

eq("File \"bs_array_test.ml\", line 41, characters 5-12", (Js_vector.filterInPlace((function (j) {
              return +(j % 2 !== 0);
            }), a$1), a$1), /* int array */[
      1,
      3,
      5
    ]);

eq("File \"bs_array_test.ml\", line 48, characters 5-12", Js_list.toVector(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ]), /* int array */[
      1,
      2,
      3
    ]);

eq("File \"bs_array_test.ml\", line 50, characters 5-12", Js_list.toVector(/* :: */[
          1,
          /* [] */0
        ]), /* int array */[1]);

id(/* [] */0);

id(/* :: */[
      1,
      /* [] */0
    ]);

id(/* :: */[
      1,
      /* :: */[
        2,
        /* :: */[
          3,
          /* :: */[
            4,
            /* :: */[
              5,
              /* [] */0
            ]
          ]
        ]
      ]
    ]);

id(Js_vector.toList(Js_vector.init(100, (function (i) {
                return i;
              }))));

function add(x, y) {
  return x + y | 0;
}

var v = Belt_Array.makeBy(3000, (function (i) {
        return i;
      }));

var u = Belt_Array.shuffle(v);

neq("File \"bs_array_test.ml\", line 61, characters 6-13", u, v);

eq("File \"bs_array_test.ml\", line 63, characters 5-12", Belt_Array.reduce(u, 0, add), Belt_Array.reduce(v, 0, add));

b("File \"bs_array_test.ml\", line 68, characters 4-11", Caml_obj.caml_equal(Belt_Array.range(0, 3), /* int array */[
          0,
          1,
          2,
          3
        ]));

b("File \"bs_array_test.ml\", line 69, characters 4-11", Caml_obj.caml_equal(Belt_Array.range(3, 0), /* int array */[]));

b("File \"bs_array_test.ml\", line 70, characters 4-11", Caml_obj.caml_equal(Belt_Array.range(3, 3), /* int array */[3]));

b("File \"bs_array_test.ml\", line 72, characters 4-11", Caml_obj.caml_equal(Belt_Array.rangeBy(0, 10, 3), /* int array */[
          0,
          3,
          6,
          9
        ]));

b("File \"bs_array_test.ml\", line 73, characters 4-11", Caml_obj.caml_equal(Belt_Array.rangeBy(0, 12, 3), /* array */[
          0,
          3,
          6,
          9,
          12
        ]));

b("File \"bs_array_test.ml\", line 74, characters 4-11", Caml_obj.caml_equal(Belt_Array.rangeBy(33, 0, 1), /* int array */[]));

b("File \"bs_array_test.ml\", line 75, characters 4-11", Caml_obj.caml_equal(Belt_Array.rangeBy(33, 0, -1), /* int array */[]));

b("File \"bs_array_test.ml\", line 76, characters 4-11", Caml_obj.caml_equal(Belt_Array.rangeBy(3, 12, -1), /* int array */[]));

b("File \"bs_array_test.ml\", line 77, characters 4-11", Caml_obj.caml_equal(Belt_Array.rangeBy(3, 3, 0), /* int array */[]));

b("File \"bs_array_test.ml\", line 78, characters 4-11", Caml_obj.caml_equal(Belt_Array.rangeBy(3, 3, 1), /* int array */[3]));

eq("File \"bs_array_test.ml\", line 83, characters 5-12", Belt_Array.reduceReverse(/* int array */[], 100, (function (prim, prim$1) {
            return prim - prim$1 | 0;
          })), 100);

eq("File \"bs_array_test.ml\", line 84, characters 5-12", Belt_Array.reduceReverse(/* int array */[
          1,
          2
        ], 100, (function (prim, prim$1) {
            return prim - prim$1 | 0;
          })), 97);

eq("File \"bs_array_test.ml\", line 85, characters 5-12", Belt_Array.reduceReverse(/* int array */[
          1,
          2,
          3,
          4
        ], 100, (function (prim, prim$1) {
            return prim - prim$1 | 0;
          })), 90);

function addone(x) {
  return x + 1 | 0;
}

function makeMatrixExn(sx, sy, init) {
  if (!(sx >= 0 && sy >= 0)) {
    throw new Error("File \"bs_array_test.ml\", line 90, characters 4-10");
  }
  var res = new Array(sx);
  for(var x = 0 ,x_finish = sx - 1 | 0; x <= x_finish; ++x){
    var initY = new Array(sy);
    for(var y = 0 ,y_finish = sy - 1 | 0; y <= y_finish; ++y){
      initY[y] = init;
    }
    res[x] = initY;
  }
  return res;
}

eq("File \"bs_array_test.ml\", line 102, characters 5-12", Belt_Array.makeBy(0, (function () {
            return 1;
          })), /* int array */[]);

eq("File \"bs_array_test.ml\", line 103, characters 5-12", Belt_Array.makeBy(3, (function (i) {
            return i;
          })), /* int array */[
      0,
      1,
      2
    ]);

eq("File \"bs_array_test.ml\", line 104, characters 5-12", makeMatrixExn(3, 4, 1), /* array */[
      /* int array */[
        1,
        1,
        1,
        1
      ],
      /* int array */[
        1,
        1,
        1,
        1
      ],
      /* int array */[
        1,
        1,
        1,
        1
      ]
    ]);

eq("File \"bs_array_test.ml\", line 107, characters 5-12", makeMatrixExn(3, 0, 0), /* array */[
      /* int array */[],
      /* int array */[],
      /* int array */[]
    ]);

eq("File \"bs_array_test.ml\", line 108, characters 5-12", makeMatrixExn(0, 3, 1), /* array */[]);

eq("File \"bs_array_test.ml\", line 109, characters 5-12", makeMatrixExn(1, 1, 1), /* array */[/* int array */[1]]);

eq("File \"bs_array_test.ml\", line 110, characters 5-12", Belt_Array.copy(/* array */[]), /* array */[]);

eq("File \"bs_array_test.ml\", line 111, characters 5-12", Belt_Array.map(/* int array */[], (function (prim) {
            return prim + 1 | 0;
          })), /* int array */[]);

eq("File \"bs_array_test.ml\", line 112, characters 5-12", Belt_Array.mapWithIndex(/* int array */[], add), /* int array */[]);

eq("File \"bs_array_test.ml\", line 113, characters 5-12", Belt_Array.mapWithIndex(/* int array */[
          1,
          2,
          3
        ], add), /* int array */[
      1,
      3,
      5
    ]);

eq("File \"bs_array_test.ml\", line 114, characters 5-12", Belt_List.ofArray(/* array */[]), /* [] */0);

eq("File \"bs_array_test.ml\", line 115, characters 5-12", Belt_List.ofArray(/* int array */[1]), /* :: */[
      1,
      /* [] */0
    ]);

eq("File \"bs_array_test.ml\", line 116, characters 5-12", Belt_List.ofArray(/* int array */[
          1,
          2,
          3
        ]), /* :: */[
      1,
      /* :: */[
        2,
        /* :: */[
          3,
          /* [] */0
        ]
      ]
    ]);

eq("File \"bs_array_test.ml\", line 117, characters 5-12", Belt_Array.map(/* int array */[
          1,
          2,
          3
        ], (function (prim) {
            return prim + 1 | 0;
          })), /* int array */[
      2,
      3,
      4
    ]);

eq("File \"bs_array_test.ml\", line 118, characters 5-12", Belt_List.toArray(/* [] */0), /* array */[]);

eq("File \"bs_array_test.ml\", line 119, characters 5-12", Belt_List.toArray(/* :: */[
          1,
          /* [] */0
        ]), /* int array */[1]);

eq("File \"bs_array_test.ml\", line 120, characters 5-12", Belt_List.toArray(/* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ]), /* int array */[
      1,
      2
    ]);

eq("File \"bs_array_test.ml\", line 121, characters 5-12", Belt_List.toArray(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ]), /* int array */[
      1,
      2,
      3
    ]);

var v$1 = Belt_Array.makeBy(10, (function (i) {
        return i;
      }));

var v0 = Belt_Array.keep(v$1, (function (x) {
        return +(x % 2 === 0);
      }));

var v1 = Belt_Array.keep(v$1, (function (x) {
        return +(x % 3 === 0);
      }));

var v2 = Belt_Array.keepMap(v$1, (function (x) {
        if (x % 2) {
          return /* None */0;
        } else {
          return /* Some */[x + 1 | 0];
        }
      }));

eq("File \"bs_array_test.ml\", line 128, characters 5-12", v0, /* array */[
      0,
      2,
      4,
      6,
      8
    ]);

eq("File \"bs_array_test.ml\", line 129, characters 5-12", v1, /* int array */[
      0,
      3,
      6,
      9
    ]);

eq("File \"bs_array_test.ml\", line 130, characters 5-12", v2, /* array */[
      1,
      3,
      5,
      7,
      9
    ]);

var a$2 = /* array */[
  1,
  2,
  3,
  4,
  5
];

eq("File \"bs_array_test.ml\", line 134, characters 5-12", Belt_Array.slice(a$2, 0, 2), /* int array */[
      1,
      2
    ]);

eq("File \"bs_array_test.ml\", line 135, characters 5-12", Belt_Array.slice(a$2, 0, 5), /* array */[
      1,
      2,
      3,
      4,
      5
    ]);

eq("File \"bs_array_test.ml\", line 136, characters 5-12", Belt_Array.slice(a$2, 0, 15), /* array */[
      1,
      2,
      3,
      4,
      5
    ]);

eq("File \"bs_array_test.ml\", line 137, characters 5-12", Belt_Array.slice(a$2, 5, 1), /* int array */[]);

eq("File \"bs_array_test.ml\", line 138, characters 5-12", Belt_Array.slice(a$2, 4, 1), /* int array */[5]);

eq("File \"bs_array_test.ml\", line 139, characters 5-12", Belt_Array.slice(a$2, -1, 1), /* int array */[5]);

eq("File \"bs_array_test.ml\", line 140, characters 5-12", Belt_Array.slice(a$2, -1, 2), /* int array */[5]);

eq("File \"bs_array_test.ml\", line 141, characters 5-12", Belt_Array.slice(a$2, -2, 1), /* int array */[4]);

eq("File \"bs_array_test.ml\", line 142, characters 5-12", Belt_Array.slice(a$2, -2, 2), /* int array */[
      4,
      5
    ]);

eq("File \"bs_array_test.ml\", line 143, characters 5-12", Belt_Array.slice(a$2, -2, 3), /* int array */[
      4,
      5
    ]);

eq("File \"bs_array_test.ml\", line 144, characters 5-12", Belt_Array.slice(a$2, -10, 3), /* int array */[
      1,
      2,
      3
    ]);

eq("File \"bs_array_test.ml\", line 145, characters 5-12", Belt_Array.slice(a$2, -10, 4), /* int array */[
      1,
      2,
      3,
      4
    ]);

eq("File \"bs_array_test.ml\", line 146, characters 5-12", Belt_Array.slice(a$2, -10, 5), /* array */[
      1,
      2,
      3,
      4,
      5
    ]);

eq("File \"bs_array_test.ml\", line 147, characters 5-12", Belt_Array.slice(a$2, -10, 6), /* array */[
      1,
      2,
      3,
      4,
      5
    ]);

eq("File \"bs_array_test.ml\", line 148, characters 5-12", Belt_Array.slice(a$2, 0, 0), /* int array */[]);

eq("File \"bs_array_test.ml\", line 149, characters 5-12", Belt_Array.slice(a$2, 0, -1), /* int array */[]);

var a$3 = Belt_Array.makeBy(10, (function (x) {
        return x;
      }));

Belt_Array.fill(a$3, 0, 3, 0);

eq("File \"bs_array_test.ml\", line 154, characters 6-13", Belt_Array.copy(a$3), /* array */[
      0,
      0,
      0,
      3,
      4,
      5,
      6,
      7,
      8,
      9
    ]);

Belt_Array.fill(a$3, 2, 8, 1);

eq("File \"bs_array_test.ml\", line 156, characters 5-12", Belt_Array.copy(a$3), /* array */[
      0,
      0,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1
    ]);

Belt_Array.fill(a$3, 8, 1, 9);

eq("File \"bs_array_test.ml\", line 158, characters 5-12", Belt_Array.copy(a$3), /* array */[
      0,
      0,
      1,
      1,
      1,
      1,
      1,
      1,
      9,
      1
    ]);

Belt_Array.fill(a$3, 8, 2, 9);

eq("File \"bs_array_test.ml\", line 160, characters 5-12", Belt_Array.copy(a$3), /* array */[
      0,
      0,
      1,
      1,
      1,
      1,
      1,
      1,
      9,
      9
    ]);

Belt_Array.fill(a$3, 8, 3, 12);

eq("File \"bs_array_test.ml\", line 162, characters 5-12", Belt_Array.copy(a$3), /* array */[
      0,
      0,
      1,
      1,
      1,
      1,
      1,
      1,
      12,
      12
    ]);

Belt_Array.fill(a$3, -2, 3, 11);

eq("File \"bs_array_test.ml\", line 164, characters 5-12", Belt_Array.copy(a$3), /* array */[
      0,
      0,
      1,
      1,
      1,
      1,
      1,
      1,
      11,
      11
    ]);

Belt_Array.fill(a$3, -3, 3, 10);

eq("File \"bs_array_test.ml\", line 166, characters 5-12", Belt_Array.copy(a$3), /* array */[
      0,
      0,
      1,
      1,
      1,
      1,
      1,
      10,
      10,
      10
    ]);

Belt_Array.fill(a$3, -3, 1, 7);

eq("File \"bs_array_test.ml\", line 168, characters 5-12", Belt_Array.copy(a$3), /* array */[
      0,
      0,
      1,
      1,
      1,
      1,
      1,
      7,
      10,
      10
    ]);

Belt_Array.fill(a$3, -13, 1, 7);

eq("File \"bs_array_test.ml\", line 170, characters 5-12", Belt_Array.copy(a$3), /* array */[
      7,
      0,
      1,
      1,
      1,
      1,
      1,
      7,
      10,
      10
    ]);

Belt_Array.fill(a$3, -13, 12, 7);

eq("File \"bs_array_test.ml\", line 172, characters 5-12", Belt_Array.copy(a$3), Belt_Array.make(10, 7));

Belt_Array.fill(a$3, 0, -1, 2);

eq("File \"bs_array_test.ml\", line 174, characters 5-12", Belt_Array.copy(a$3), Belt_Array.make(10, 7));

var a0 = Belt_Array.makeBy(10, (function (x) {
        return x;
      }));

var b0 = Belt_Array.make(10, 3);

Belt_Array.blit(a0, 1, b0, 2, 5);

eq("File \"bs_array_test.ml\", line 180, characters 5-12", Belt_Array.copy(b0), /* array */[
      3,
      3,
      1,
      2,
      3,
      4,
      5,
      3,
      3,
      3
    ]);

Belt_Array.blit(a0, -1, b0, 2, 5);

eq("File \"bs_array_test.ml\", line 183, characters 5-12", Belt_Array.copy(b0), /* array */[
      3,
      3,
      9,
      2,
      3,
      4,
      5,
      3,
      3,
      3
    ]);

Belt_Array.blit(a0, -1, b0, -2, 5);

eq("File \"bs_array_test.ml\", line 186, characters 5-12", Belt_Array.copy(b0), /* array */[
      3,
      3,
      9,
      2,
      3,
      4,
      5,
      3,
      9,
      3
    ]);

Belt_Array.blit(a0, -2, b0, -2, 2);

eq("File \"bs_array_test.ml\", line 189, characters 5-12", Belt_Array.copy(b0), /* array */[
      3,
      3,
      9,
      2,
      3,
      4,
      5,
      3,
      8,
      9
    ]);

Belt_Array.blit(a0, -11, b0, -11, 100);

eq("File \"bs_array_test.ml\", line 192, characters 5-12", Belt_Array.copy(b0), a0);

Belt_Array.blit(a0, -11, b0, -11, 2);

eq("File \"bs_array_test.ml\", line 194, characters 5-12", Belt_Array.copy(b0), a0);

var aa = Belt_Array.makeBy(10, (function (x) {
        return x;
      }));

Belt_Array.blit(aa, -1, aa, 1, 2);

eq("File \"bs_array_test.ml\", line 197, characters 5-12", Belt_Array.copy(aa), /* array */[
      0,
      9,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9
    ]);

Belt_Array.blit(aa, -2, aa, 1, 2);

eq("File \"bs_array_test.ml\", line 199, characters 5-12", Belt_Array.copy(aa), /* array */[
      0,
      8,
      9,
      3,
      4,
      5,
      6,
      7,
      8,
      9
    ]);

Belt_Array.blit(aa, -5, aa, 4, 3);

eq("File \"bs_array_test.ml\", line 201, characters 5-12", Belt_Array.copy(aa), /* array */[
      0,
      8,
      9,
      3,
      5,
      6,
      7,
      7,
      8,
      9
    ]);

Belt_Array.blit(aa, 4, aa, 5, 3);

eq("File \"bs_array_test.ml\", line 203, characters 5-12", Belt_Array.copy(aa), /* array */[
      0,
      8,
      9,
      3,
      5,
      5,
      6,
      7,
      8,
      9
    ]);

function id$1(_, x) {
  var u = Belt_Array.copy(x);
  return eq("File \"bs_array_test.ml\", line 206, characters 5-12", Belt_Array.reverse(x), (Belt_Array.reverseInPlace(u), u));
}

id$1("File \"bs_array_test.ml\", line 211, characters 5-12", /* array */[]);

id$1("File \"bs_array_test.ml\", line 212, characters 5-12", /* int array */[1]);

id$1("File \"bs_array_test.ml\", line 213, characters 5-12", /* int array */[
      1,
      2
    ]);

id$1("File \"bs_array_test.ml\", line 214, characters 5-12", /* int array */[
      1,
      2,
      3
    ]);

id$1("File \"bs_array_test.ml\", line 215, characters 5-12", /* int array */[
      1,
      2,
      3,
      4
    ]);

function every2(xs, ys) {
  var partial_arg = Belt_List.toArray(ys);
  var partial_arg$1 = Belt_List.toArray(xs);
  return (function (param) {
      return Belt_Array.every2(partial_arg$1, partial_arg, param);
    });
}

function some2(xs, ys) {
  var partial_arg = Belt_List.toArray(ys);
  var partial_arg$1 = Belt_List.toArray(xs);
  return (function (param) {
      return Belt_Array.some2(partial_arg$1, partial_arg, param);
    });
}

eq("File \"bs_array_test.ml\", line 225, characters 5-12", every2(/* [] */0, /* :: */[
            1,
            /* [] */0
          ])((function (x, y) {
            return +(x > y);
          })), /* true */1);

eq("File \"bs_array_test.ml\", line 226, characters 5-12", every2(/* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ], /* :: */[
            1,
            /* [] */0
          ])((function (x, y) {
            return +(x > y);
          })), /* true */1);

eq("File \"bs_array_test.ml\", line 227, characters 5-12", every2(/* :: */[
            2,
            /* [] */0
          ], /* :: */[
            1,
            /* [] */0
          ])((function (x, y) {
            return +(x > y);
          })), /* true */1);

eq("File \"bs_array_test.ml\", line 228, characters 5-12", every2(/* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ], /* :: */[
            1,
            /* :: */[
              4,
              /* [] */0
            ]
          ])((function (x, y) {
            return +(x > y);
          })), /* false */0);

eq("File \"bs_array_test.ml\", line 229, characters 5-12", every2(/* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ], /* :: */[
            1,
            /* :: */[
              0,
              /* [] */0
            ]
          ])((function (x, y) {
            return +(x > y);
          })), /* true */1);

eq("File \"bs_array_test.ml\", line 230, characters 5-12", some2(/* [] */0, /* :: */[
            1,
            /* [] */0
          ])((function (x, y) {
            return +(x > y);
          })), /* false */0);

eq("File \"bs_array_test.ml\", line 231, characters 5-12", some2(/* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ], /* :: */[
            1,
            /* [] */0
          ])((function (x, y) {
            return +(x > y);
          })), /* true */1);

eq("File \"bs_array_test.ml\", line 232, characters 5-12", some2(/* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ], /* :: */[
            1,
            /* :: */[
              4,
              /* [] */0
            ]
          ])((function (x, y) {
            return +(x > y);
          })), /* true */1);

eq("File \"bs_array_test.ml\", line 233, characters 5-12", some2(/* :: */[
            0,
            /* :: */[
              3,
              /* [] */0
            ]
          ], /* :: */[
            1,
            /* :: */[
              4,
              /* [] */0
            ]
          ])((function (x, y) {
            return +(x > y);
          })), /* false */0);

eq("File \"bs_array_test.ml\", line 234, characters 5-12", some2(/* :: */[
            0,
            /* :: */[
              3,
              /* [] */0
            ]
          ], /* :: */[
            3,
            /* :: */[
              2,
              /* [] */0
            ]
          ])((function (x, y) {
            return +(x > y);
          })), /* true */1);

eq("File \"bs_array_test.ml\", line 239, characters 5-12", Belt_Array.concat(/* int array */[], /* int array */[
          1,
          2,
          3
        ]), /* int array */[
      1,
      2,
      3
    ]);

eq("File \"bs_array_test.ml\", line 240, characters 5-12", Belt_Array.concat(/* array */[], /* array */[]), /* array */[]);

eq("File \"bs_array_test.ml\", line 241, characters 5-12", Belt_Array.concat(/* int array */[
          3,
          2
        ], /* int array */[
          1,
          2,
          3
        ]), /* array */[
      3,
      2,
      1,
      2,
      3
    ]);

eq("File \"bs_array_test.ml\", line 242, characters 5-12", Belt_Array.concatMany(/* array */[
          /* int array */[
            3,
            2
          ],
          /* int array */[
            1,
            2,
            3
          ]
        ]), /* array */[
      3,
      2,
      1,
      2,
      3
    ]);

eq("File \"bs_array_test.ml\", line 243, characters 5-12", Belt_Array.concatMany(/* array */[
          /* int array */[
            3,
            2
          ],
          /* int array */[
            1,
            2,
            3
          ],
          /* int array */[],
          /* int array */[0]
        ]), /* array */[
      3,
      2,
      1,
      2,
      3,
      0
    ]);

eq("File \"bs_array_test.ml\", line 244, characters 5-12", Belt_Array.concatMany(/* array */[
          /* int array */[],
          /* int array */[
            3,
            2
          ],
          /* int array */[
            1,
            2,
            3
          ],
          /* int array */[],
          /* int array */[0]
        ]), /* array */[
      3,
      2,
      1,
      2,
      3,
      0
    ]);

eq("File \"bs_array_test.ml\", line 245, characters 5-12", Belt_Array.concatMany(/* array */[
          /* array */[],
          /* array */[]
        ]), /* array */[]);

Mt.from_pair_suites("File \"bs_array_test.ml\", line 247, characters 23-30", suites[0]);

var A = 0;

var L = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.neq = neq;
exports.A = A;
exports.L = L;
exports.add = add;
exports.addone = addone;
exports.makeMatrixExn = makeMatrixExn;
exports.id = id$1;
/*  Not a pure module */

'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Js_list = require("../../lib/js/js_list.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Belt_List = require("../../lib/js/belt_List.js");
var Js_vector = require("../../lib/js/js_vector.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

function $$throw(loc, x) {
  return Mt.throw_suites(test_id, suites, loc, x);
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

var v = /* int array */[
  1,
  2
];

eq("File \"bs_array_test.ml\", line 25, characters 5-12", /* tuple */[
      Belt_Array.get(v, 0),
      Belt_Array.get(v, 1),
      Belt_Array.get(v, 2),
      Belt_Array.get(v, 3),
      Belt_Array.get(v, -1)
    ], /* tuple */[
      /* Some */[1],
      /* Some */[2],
      /* None */0,
      /* None */0,
      /* None */0
    ]);

$$throw("File \"bs_array_test.ml\", line 28, characters 8-15", (function () {
        Belt_Array.getExn(/* int array */[
              0,
              1
            ], -1);
        return /* () */0;
      }));

$$throw("File \"bs_array_test.ml\", line 29, characters 8-15", (function () {
        Belt_Array.getExn(/* int array */[
              0,
              1
            ], 2);
        return /* () */0;
      }));

var partial_arg = /* int array */[
  0,
  1
];

function f(param) {
  return Belt_Array.getExn(partial_arg, param);
}

b("File \"bs_array_test.ml\", line 30, characters 4-11", Caml_obj.caml_equal(/* tuple */[
          Curry._1(f, 0),
          Curry._1(f, 1)
        ], /* tuple */[
          0,
          1
        ]));

$$throw("File \"bs_array_test.ml\", line 31, characters 8-15", (function () {
        return Belt_Array.setExn(/* int array */[
                    0,
                    1
                  ], -1, 0);
      }));

$$throw("File \"bs_array_test.ml\", line 32, characters 8-15", (function () {
        return Belt_Array.setExn(/* int array */[
                    0,
                    1
                  ], 2, 0);
      }));

b("File \"bs_array_test.ml\", line 33, characters 4-11", 1 - Belt_Array.set(/* int array */[
          1,
          2
        ], 2, 0));

var v$1 = /* int array */[
  1,
  2
];

if (!Belt_Array.set(v$1, 0, 0)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_array_test.ml",
          34,
          33
        ]
      ];
}

b("File \"bs_array_test.ml\", line 34, characters 4-11", +(Belt_Array.getExn(v$1, 0) === 0));

var v$2 = /* int array */[
  1,
  2
];

if (!Belt_Array.set(v$2, 1, 0)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "bs_array_test.ml",
          35,
          32
        ]
      ];
}

b("File \"bs_array_test.ml\", line 35, characters 4-11", +(Belt_Array.getExn(v$2, 1) === 0));

var v$3 = /* int array */[
  1,
  2
];

b("File \"bs_array_test.ml\", line 36, characters 4-11", (Belt_Array.setExn(v$3, 0, 0), +(Belt_Array.getExn(v$3, 0) === 0)));

var v$4 = /* int array */[
  1,
  2
];

b("File \"bs_array_test.ml\", line 37, characters 4-11", (Belt_Array.setExn(v$4, 1, 0), +(Belt_Array.getExn(v$4, 1) === 0)));

function id(x) {
  return eq("File \"bs_array_test.ml\", line 40, characters 5-12", Js_vector.toList(Js_list.toVector(x)), x);
}

eq("File \"bs_array_test.ml\", line 44, characters 5-12", Js_list.toVector(/* :: */[
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

eq("File \"bs_array_test.ml\", line 45, characters 6-13", Js_vector.map((function (x) {
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

eq("File \"bs_array_test.ml\", line 48, characters 5-12", Caml_array.caml_make_vect(5, 3), /* array */[
      3,
      3,
      3,
      3,
      3
    ]);

var a = Js_vector.init(5, (function (i) {
        return i + 1 | 0;
      }));

eq("File \"bs_array_test.ml\", line 50, characters 5-12", (Js_vector.filterInPlace((function (j) {
              return +(j % 2 === 0);
            }), a), a), /* int array */[
      2,
      4
    ]);

var a$1 = Js_vector.init(5, (function (i) {
        return i + 1 | 0;
      }));

eq("File \"bs_array_test.ml\", line 57, characters 5-12", (Js_vector.filterInPlace((function (j) {
              return +(j % 2 !== 0);
            }), a$1), a$1), /* int array */[
      1,
      3,
      5
    ]);

eq("File \"bs_array_test.ml\", line 64, characters 5-12", Js_list.toVector(/* :: */[
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

eq("File \"bs_array_test.ml\", line 66, characters 5-12", Js_list.toVector(/* :: */[
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

var v$5 = Belt_Array.makeBy(3000, (function (i) {
        return i;
      }));

var u = Belt_Array.shuffle(v$5);

neq("File \"bs_array_test.ml\", line 77, characters 6-13", u, v$5);

eq("File \"bs_array_test.ml\", line 79, characters 5-12", Belt_Array.reduce(u, 0, add), Belt_Array.reduce(v$5, 0, add));

b("File \"bs_array_test.ml\", line 84, characters 4-11", Caml_obj.caml_equal(Belt_Array.range(0, 3), /* int array */[
          0,
          1,
          2,
          3
        ]));

b("File \"bs_array_test.ml\", line 85, characters 4-11", Caml_obj.caml_equal(Belt_Array.range(3, 0), /* int array */[]));

b("File \"bs_array_test.ml\", line 86, characters 4-11", Caml_obj.caml_equal(Belt_Array.range(3, 3), /* int array */[3]));

b("File \"bs_array_test.ml\", line 88, characters 4-11", Caml_obj.caml_equal(Belt_Array.rangeBy(0, 10, 3), /* int array */[
          0,
          3,
          6,
          9
        ]));

b("File \"bs_array_test.ml\", line 89, characters 4-11", Caml_obj.caml_equal(Belt_Array.rangeBy(0, 12, 3), /* array */[
          0,
          3,
          6,
          9,
          12
        ]));

b("File \"bs_array_test.ml\", line 90, characters 4-11", Caml_obj.caml_equal(Belt_Array.rangeBy(33, 0, 1), /* int array */[]));

b("File \"bs_array_test.ml\", line 91, characters 4-11", Caml_obj.caml_equal(Belt_Array.rangeBy(33, 0, -1), /* int array */[]));

b("File \"bs_array_test.ml\", line 92, characters 4-11", Caml_obj.caml_equal(Belt_Array.rangeBy(3, 12, -1), /* int array */[]));

b("File \"bs_array_test.ml\", line 93, characters 4-11", Caml_obj.caml_equal(Belt_Array.rangeBy(3, 3, 0), /* int array */[]));

b("File \"bs_array_test.ml\", line 94, characters 4-11", Caml_obj.caml_equal(Belt_Array.rangeBy(3, 3, 1), /* int array */[3]));

eq("File \"bs_array_test.ml\", line 99, characters 5-12", Belt_Array.reduceReverse(/* int array */[], 100, (function (prim, prim$1) {
            return prim - prim$1 | 0;
          })), 100);

eq("File \"bs_array_test.ml\", line 100, characters 5-12", Belt_Array.reduceReverse(/* int array */[
          1,
          2
        ], 100, (function (prim, prim$1) {
            return prim - prim$1 | 0;
          })), 97);

eq("File \"bs_array_test.ml\", line 101, characters 5-12", Belt_Array.reduceReverse(/* int array */[
          1,
          2,
          3,
          4
        ], 100, (function (prim, prim$1) {
            return prim - prim$1 | 0;
          })), 90);

b("File \"bs_array_test.ml\", line 102, characters 4-11", +(Belt_Array.reduceReverse2(/* int array */[
            1,
            2,
            3
          ], /* int array */[
            1,
            2
          ], 0, (function (acc, x, y) {
              return (acc + x | 0) + y | 0;
            })) === 6));

function addone(x) {
  return x + 1 | 0;
}

function makeMatrixExn(sx, sy, init) {
  if (!(sx >= 0 && sy >= 0)) {
    throw new Error("File \"bs_array_test.ml\", line 108, characters 4-10");
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

eq("File \"bs_array_test.ml\", line 120, characters 5-12", Belt_Array.makeBy(0, (function () {
            return 1;
          })), /* int array */[]);

eq("File \"bs_array_test.ml\", line 121, characters 5-12", Belt_Array.makeBy(3, (function (i) {
            return i;
          })), /* int array */[
      0,
      1,
      2
    ]);

eq("File \"bs_array_test.ml\", line 122, characters 5-12", makeMatrixExn(3, 4, 1), /* array */[
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

eq("File \"bs_array_test.ml\", line 125, characters 5-12", makeMatrixExn(3, 0, 0), /* array */[
      /* int array */[],
      /* int array */[],
      /* int array */[]
    ]);

eq("File \"bs_array_test.ml\", line 126, characters 5-12", makeMatrixExn(0, 3, 1), /* array */[]);

eq("File \"bs_array_test.ml\", line 127, characters 5-12", makeMatrixExn(1, 1, 1), /* array */[/* int array */[1]]);

eq("File \"bs_array_test.ml\", line 128, characters 5-12", Belt_Array.copy(/* array */[]), /* array */[]);

eq("File \"bs_array_test.ml\", line 129, characters 5-12", Belt_Array.map(/* int array */[], (function (prim) {
            return prim + 1 | 0;
          })), /* int array */[]);

eq("File \"bs_array_test.ml\", line 130, characters 5-12", Belt_Array.mapWithIndex(/* int array */[], add), /* int array */[]);

eq("File \"bs_array_test.ml\", line 131, characters 5-12", Belt_Array.mapWithIndex(/* int array */[
          1,
          2,
          3
        ], add), /* int array */[
      1,
      3,
      5
    ]);

eq("File \"bs_array_test.ml\", line 132, characters 5-12", Belt_List.ofArray(/* array */[]), /* [] */0);

eq("File \"bs_array_test.ml\", line 133, characters 5-12", Belt_List.ofArray(/* int array */[1]), /* :: */[
      1,
      /* [] */0
    ]);

eq("File \"bs_array_test.ml\", line 134, characters 5-12", Belt_List.ofArray(/* int array */[
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

eq("File \"bs_array_test.ml\", line 135, characters 5-12", Belt_Array.map(/* int array */[
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

eq("File \"bs_array_test.ml\", line 136, characters 5-12", Belt_List.toArray(/* [] */0), /* array */[]);

eq("File \"bs_array_test.ml\", line 137, characters 5-12", Belt_List.toArray(/* :: */[
          1,
          /* [] */0
        ]), /* int array */[1]);

eq("File \"bs_array_test.ml\", line 138, characters 5-12", Belt_List.toArray(/* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ]), /* int array */[
      1,
      2
    ]);

eq("File \"bs_array_test.ml\", line 139, characters 5-12", Belt_List.toArray(/* :: */[
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

var v$6 = Belt_Array.makeBy(10, (function (i) {
        return i;
      }));

var v0 = Belt_Array.keep(v$6, (function (x) {
        return +(x % 2 === 0);
      }));

var v1 = Belt_Array.keep(v$6, (function (x) {
        return +(x % 3 === 0);
      }));

var v2 = Belt_Array.keepMap(v$6, (function (x) {
        if (x % 2) {
          return /* None */0;
        } else {
          return /* Some */[x + 1 | 0];
        }
      }));

eq("File \"bs_array_test.ml\", line 146, characters 5-12", v0, /* array */[
      0,
      2,
      4,
      6,
      8
    ]);

eq("File \"bs_array_test.ml\", line 147, characters 5-12", v1, /* int array */[
      0,
      3,
      6,
      9
    ]);

eq("File \"bs_array_test.ml\", line 148, characters 5-12", v2, /* array */[
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

eq("File \"bs_array_test.ml\", line 152, characters 5-12", Belt_Array.slice(a$2, 0, 2), /* int array */[
      1,
      2
    ]);

eq("File \"bs_array_test.ml\", line 153, characters 5-12", Belt_Array.slice(a$2, 0, 5), /* array */[
      1,
      2,
      3,
      4,
      5
    ]);

eq("File \"bs_array_test.ml\", line 154, characters 5-12", Belt_Array.slice(a$2, 0, 15), /* array */[
      1,
      2,
      3,
      4,
      5
    ]);

eq("File \"bs_array_test.ml\", line 155, characters 5-12", Belt_Array.slice(a$2, 5, 1), /* int array */[]);

eq("File \"bs_array_test.ml\", line 156, characters 5-12", Belt_Array.slice(a$2, 4, 1), /* int array */[5]);

eq("File \"bs_array_test.ml\", line 157, characters 5-12", Belt_Array.slice(a$2, -1, 1), /* int array */[5]);

eq("File \"bs_array_test.ml\", line 158, characters 5-12", Belt_Array.slice(a$2, -1, 2), /* int array */[5]);

eq("File \"bs_array_test.ml\", line 159, characters 5-12", Belt_Array.slice(a$2, -2, 1), /* int array */[4]);

eq("File \"bs_array_test.ml\", line 160, characters 5-12", Belt_Array.slice(a$2, -2, 2), /* int array */[
      4,
      5
    ]);

eq("File \"bs_array_test.ml\", line 161, characters 5-12", Belt_Array.slice(a$2, -2, 3), /* int array */[
      4,
      5
    ]);

eq("File \"bs_array_test.ml\", line 162, characters 5-12", Belt_Array.slice(a$2, -10, 3), /* int array */[
      1,
      2,
      3
    ]);

eq("File \"bs_array_test.ml\", line 163, characters 5-12", Belt_Array.slice(a$2, -10, 4), /* int array */[
      1,
      2,
      3,
      4
    ]);

eq("File \"bs_array_test.ml\", line 164, characters 5-12", Belt_Array.slice(a$2, -10, 5), /* array */[
      1,
      2,
      3,
      4,
      5
    ]);

eq("File \"bs_array_test.ml\", line 165, characters 5-12", Belt_Array.slice(a$2, -10, 6), /* array */[
      1,
      2,
      3,
      4,
      5
    ]);

eq("File \"bs_array_test.ml\", line 166, characters 5-12", Belt_Array.slice(a$2, 0, 0), /* int array */[]);

eq("File \"bs_array_test.ml\", line 167, characters 5-12", Belt_Array.slice(a$2, 0, -1), /* int array */[]);

var a$3 = Belt_Array.makeBy(10, (function (x) {
        return x;
      }));

Belt_Array.fill(a$3, 0, 3, 0);

eq("File \"bs_array_test.ml\", line 172, characters 6-13", Belt_Array.copy(a$3), /* array */[
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

eq("File \"bs_array_test.ml\", line 174, characters 5-12", Belt_Array.copy(a$3), /* array */[
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

eq("File \"bs_array_test.ml\", line 176, characters 5-12", Belt_Array.copy(a$3), /* array */[
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

eq("File \"bs_array_test.ml\", line 178, characters 5-12", Belt_Array.copy(a$3), /* array */[
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

eq("File \"bs_array_test.ml\", line 180, characters 5-12", Belt_Array.copy(a$3), /* array */[
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

eq("File \"bs_array_test.ml\", line 182, characters 5-12", Belt_Array.copy(a$3), /* array */[
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

eq("File \"bs_array_test.ml\", line 184, characters 5-12", Belt_Array.copy(a$3), /* array */[
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

eq("File \"bs_array_test.ml\", line 186, characters 5-12", Belt_Array.copy(a$3), /* array */[
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

eq("File \"bs_array_test.ml\", line 188, characters 5-12", Belt_Array.copy(a$3), /* array */[
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

eq("File \"bs_array_test.ml\", line 190, characters 5-12", Belt_Array.copy(a$3), Belt_Array.make(10, 7));

Belt_Array.fill(a$3, 0, -1, 2);

eq("File \"bs_array_test.ml\", line 192, characters 5-12", Belt_Array.copy(a$3), Belt_Array.make(10, 7));

var b$1 = /* int array */[
  1,
  2,
  3
];

Belt_Array.fill(b$1, 0, 0, 0);

eq("File \"bs_array_test.ml\", line 195, characters 5-12", b$1, /* int array */[
      1,
      2,
      3
    ]);

Belt_Array.fill(b$1, 4, 1, 0);

eq("File \"bs_array_test.ml\", line 197, characters 5-12", b$1, /* int array */[
      1,
      2,
      3
    ]);

var a0 = Belt_Array.makeBy(10, (function (x) {
        return x;
      }));

var b0 = Belt_Array.make(10, 3);

Belt_Array.blit(a0, 1, b0, 2, 5);

eq("File \"bs_array_test.ml\", line 203, characters 5-12", Belt_Array.copy(b0), /* array */[
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

eq("File \"bs_array_test.ml\", line 206, characters 5-12", Belt_Array.copy(b0), /* array */[
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

eq("File \"bs_array_test.ml\", line 209, characters 5-12", Belt_Array.copy(b0), /* array */[
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

eq("File \"bs_array_test.ml\", line 212, characters 5-12", Belt_Array.copy(b0), /* array */[
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

eq("File \"bs_array_test.ml\", line 215, characters 5-12", Belt_Array.copy(b0), a0);

Belt_Array.blit(a0, -11, b0, -11, 2);

eq("File \"bs_array_test.ml\", line 217, characters 5-12", Belt_Array.copy(b0), a0);

var aa = Belt_Array.makeBy(10, (function (x) {
        return x;
      }));

Belt_Array.blit(aa, -1, aa, 1, 2);

eq("File \"bs_array_test.ml\", line 220, characters 5-12", Belt_Array.copy(aa), /* array */[
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

eq("File \"bs_array_test.ml\", line 222, characters 5-12", Belt_Array.copy(aa), /* array */[
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

eq("File \"bs_array_test.ml\", line 224, characters 5-12", Belt_Array.copy(aa), /* array */[
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

eq("File \"bs_array_test.ml\", line 226, characters 5-12", Belt_Array.copy(aa), /* array */[
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

eq("File \"bs_array_test.ml\", line 227, characters 5-12", Belt_Array.make(0, 3), /* int array */[]);

eq("File \"bs_array_test.ml\", line 228, characters 5-12", Belt_Array.make(-1, 3), /* int array */[]);

var c = /* int array */[
  0,
  1,
  2
];

Belt_Array.blit(c, 4, c, 1, 1);

eq("File \"bs_array_test.ml\", line 231, characters 5-12", c, /* int array */[
      0,
      1,
      2
    ]);

eq("File \"bs_array_test.ml\", line 234, characters 5-12", Belt_Array.zip(/* int array */[
          1,
          2,
          3
        ], /* int array */[
          2,
          3,
          4,
          1
        ]), /* array */[
      /* tuple */[
        1,
        2
      ],
      /* tuple */[
        2,
        3
      ],
      /* tuple */[
        3,
        4
      ]
    ]);

eq("File \"bs_array_test.ml\", line 235, characters 5-12", Belt_Array.zip(/* int array */[
          2,
          3,
          4,
          1
        ], /* int array */[
          1,
          2,
          3
        ]), /* array */[
      /* tuple */[
        2,
        1
      ],
      /* tuple */[
        3,
        2
      ],
      /* tuple */[
        4,
        3
      ]
    ]);

eq("File \"bs_array_test.ml\", line 236, characters 5-12", Belt_Array.zipBy(/* int array */[
          2,
          3,
          4,
          1
        ], /* int array */[
          1,
          2,
          3
        ], (function (prim, prim$1) {
            return prim - prim$1 | 0;
          })), /* int array */[
      1,
      1,
      1
    ]);

eq("File \"bs_array_test.ml\", line 237, characters 5-12", Belt_Array.zipBy(/* int array */[
          1,
          2,
          3
        ], /* int array */[
          2,
          3,
          4,
          1
        ], (function (prim, prim$1) {
            return prim - prim$1 | 0;
          })), Belt_Array.map(/* int array */[
          1,
          1,
          1
        ], (function (x) {
            return -x | 0;
          })));

function sumUsingForEach(xs) {
  var v = [0];
  Belt_Array.forEach(xs, (function (x) {
          v[0] = v[0] + x | 0;
          return /* () */0;
        }));
  return v[0];
}

eq("File \"bs_array_test.ml\", line 245, characters 5-12", sumUsingForEach(/* array */[
          0,
          1,
          2,
          3,
          4
        ]), 10);

b("File \"bs_array_test.ml\", line 246, characters 4-11", 1 - Belt_Array.every(/* array */[
          0,
          1,
          2,
          3,
          4
        ], (function (x) {
            return +(x > 2);
          })));

b("File \"bs_array_test.ml\", line 247, characters 4-11", Belt_Array.some(/* int array */[
          1,
          3,
          7,
          8
        ], (function (x) {
            return +(x % 2 === 0);
          })));

b("File \"bs_array_test.ml\", line 248, characters 4-11", 1 - Belt_Array.some(/* int array */[
          1,
          3,
          7
        ], (function (x) {
            return +(x % 2 === 0);
          })));

b("File \"bs_array_test.ml\", line 249, characters 4-11", 1 - Belt_Array.eq(/* int array */[
          0,
          1
        ], /* int array */[1], Caml_obj.caml_equal));

var c$1 = [0];

b("File \"bs_array_test.ml\", line 250, characters 4-11", (Belt_Array.forEachWithIndex(/* int array */[
            1,
            1,
            1
          ], (function (i, v) {
              c$1[0] = (c$1[0] + i | 0) + v | 0;
              return /* () */0;
            })), +(c$1[0] === 6)));

function id$1(_, x) {
  var u = Belt_Array.copy(x);
  return eq("File \"bs_array_test.ml\", line 260, characters 5-12", Belt_Array.reverse(x), (Belt_Array.reverseInPlace(u), u));
}

id$1("File \"bs_array_test.ml\", line 265, characters 5-12", /* array */[]);

id$1("File \"bs_array_test.ml\", line 266, characters 5-12", /* int array */[1]);

id$1("File \"bs_array_test.ml\", line 267, characters 5-12", /* int array */[
      1,
      2
    ]);

id$1("File \"bs_array_test.ml\", line 268, characters 5-12", /* int array */[
      1,
      2,
      3
    ]);

id$1("File \"bs_array_test.ml\", line 269, characters 5-12", /* int array */[
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

eq("File \"bs_array_test.ml\", line 279, characters 5-12", every2(/* [] */0, /* :: */[
            1,
            /* [] */0
          ])((function (x, y) {
            return +(x > y);
          })), /* true */1);

eq("File \"bs_array_test.ml\", line 280, characters 5-12", every2(/* :: */[
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

eq("File \"bs_array_test.ml\", line 281, characters 5-12", every2(/* :: */[
            2,
            /* [] */0
          ], /* :: */[
            1,
            /* [] */0
          ])((function (x, y) {
            return +(x > y);
          })), /* true */1);

eq("File \"bs_array_test.ml\", line 282, characters 5-12", every2(/* :: */[
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

eq("File \"bs_array_test.ml\", line 283, characters 5-12", every2(/* :: */[
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

eq("File \"bs_array_test.ml\", line 284, characters 5-12", some2(/* [] */0, /* :: */[
            1,
            /* [] */0
          ])((function (x, y) {
            return +(x > y);
          })), /* false */0);

eq("File \"bs_array_test.ml\", line 285, characters 5-12", some2(/* :: */[
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

eq("File \"bs_array_test.ml\", line 286, characters 5-12", some2(/* :: */[
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

eq("File \"bs_array_test.ml\", line 287, characters 5-12", some2(/* :: */[
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

eq("File \"bs_array_test.ml\", line 288, characters 5-12", some2(/* :: */[
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

eq("File \"bs_array_test.ml\", line 293, characters 5-12", Belt_Array.concat(/* int array */[], /* int array */[
          1,
          2,
          3
        ]), /* int array */[
      1,
      2,
      3
    ]);

eq("File \"bs_array_test.ml\", line 294, characters 5-12", Belt_Array.concat(/* array */[], /* array */[]), /* array */[]);

eq("File \"bs_array_test.ml\", line 295, characters 5-12", Belt_Array.concat(/* int array */[
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

eq("File \"bs_array_test.ml\", line 296, characters 5-12", Belt_Array.concatMany(/* array */[
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

eq("File \"bs_array_test.ml\", line 297, characters 5-12", Belt_Array.concatMany(/* array */[
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

eq("File \"bs_array_test.ml\", line 298, characters 5-12", Belt_Array.concatMany(/* array */[
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

eq("File \"bs_array_test.ml\", line 299, characters 5-12", Belt_Array.concatMany(/* array */[
          /* array */[],
          /* array */[]
        ]), /* array */[]);

b("File \"bs_array_test.ml\", line 302, characters 4-11", +(Belt_Array.cmp(/* int array */[
            1,
            2,
            3
          ], /* int array */[
            0,
            1,
            2,
            3
          ], Caml_obj.caml_compare) < 0));

b("File \"bs_array_test.ml\", line 303, characters 4-11", +(Belt_Array.cmp(/* int array */[
            0,
            1,
            2,
            3
          ], /* int array */[
            1,
            2,
            3
          ], Caml_obj.caml_compare) > 0));

b("File \"bs_array_test.ml\", line 304, characters 4-11", +(Belt_Array.cmp(/* int array */[
            1,
            2,
            3
          ], /* int array */[
            0,
            1,
            2
          ], Caml_primitive.caml_int_compare) > 0));

b("File \"bs_array_test.ml\", line 305, characters 4-11", +(Belt_Array.cmp(/* int array */[
            1,
            2,
            3
          ], /* int array */[
            1,
            2,
            3
          ], Caml_primitive.caml_int_compare) === 0));

b("File \"bs_array_test.ml\", line 306, characters 4-11", +(Belt_Array.cmp(/* int array */[
            1,
            2,
            4
          ], /* int array */[
            1,
            2,
            3
          ], Caml_primitive.caml_int_compare) > 0));

Mt.from_pair_suites("File \"bs_array_test.ml\", line 309, characters 23-30", suites[0]);

var A = 0;

var L = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.$$throw = $$throw;
exports.neq = neq;
exports.A = A;
exports.L = L;
exports.add = add;
exports.addone = addone;
exports.makeMatrixExn = makeMatrixExn;
exports.sumUsingForEach = sumUsingForEach;
exports.id = id$1;
/*  Not a pure module */

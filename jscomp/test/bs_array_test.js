'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Bs_List = require("../../lib/js/bs_List.js");
var Js_list = require("../../lib/js/js_list.js");
var Bs_Array = require("../../lib/js/bs_Array.js");
var Js_vector = require("../../lib/js/js_vector.js");
var Caml_array = require("../../lib/js/caml_array.js");

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
  return eq("File \"bs_array_test.ml\", line 26, characters 5-12", Js_vector.toList(Js_list.toVector(x)), x);
}

eq("File \"bs_array_test.ml\", line 30, characters 5-12", Js_list.toVector(/* :: */[
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

eq("File \"bs_array_test.ml\", line 31, characters 6-13", Js_vector.map((function (x) {
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

eq("File \"bs_array_test.ml\", line 34, characters 5-12", Caml_array.caml_make_vect(5, 3), /* array */[
      3,
      3,
      3,
      3,
      3
    ]);

var a = Js_vector.init(5, (function (i) {
        return i + 1 | 0;
      }));

eq("File \"bs_array_test.ml\", line 36, characters 5-12", (Js_vector.filterInPlace((function (j) {
              return +(j % 2 === 0);
            }), a), a), /* int array */[
      2,
      4
    ]);

var a$1 = Js_vector.init(5, (function (i) {
        return i + 1 | 0;
      }));

eq("File \"bs_array_test.ml\", line 43, characters 5-12", (Js_vector.filterInPlace((function (j) {
              return +(j % 2 !== 0);
            }), a$1), a$1), /* int array */[
      1,
      3,
      5
    ]);

eq("File \"bs_array_test.ml\", line 50, characters 5-12", Js_list.toVector(/* :: */[
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

eq("File \"bs_array_test.ml\", line 52, characters 5-12", Js_list.toVector(/* :: */[
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

var v = Bs_Array.makeBy(3000, (function (i) {
        return i;
      }));

var u = Bs_Array.shuffle(v);

neq("File \"bs_array_test.ml\", line 63, characters 6-13", u, v);

eq("File \"bs_array_test.ml\", line 65, characters 5-12", Bs_Array.reduce(u, 0, add), Bs_Array.reduce(v, 0, add));

eq("File \"bs_array_test.ml\", line 68, characters 5-12", Bs_Array.reduceReverse(/* int array */[], 100, (function (prim, prim$1) {
            return prim - prim$1 | 0;
          })), 100);

eq("File \"bs_array_test.ml\", line 69, characters 5-12", Bs_Array.reduceReverse(/* int array */[
          1,
          2
        ], 100, (function (prim, prim$1) {
            return prim - prim$1 | 0;
          })), 97);

eq("File \"bs_array_test.ml\", line 70, characters 5-12", Bs_Array.reduceReverse(/* int array */[
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
    throw new Error("File \"bs_array_test.ml\", line 75, characters 4-10");
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

eq("File \"bs_array_test.ml\", line 87, characters 5-12", Bs_Array.makeBy(0, (function () {
            return 1;
          })), /* int array */[]);

eq("File \"bs_array_test.ml\", line 88, characters 5-12", Bs_Array.makeBy(3, (function (i) {
            return i;
          })), /* int array */[
      0,
      1,
      2
    ]);

eq("File \"bs_array_test.ml\", line 89, characters 5-12", makeMatrixExn(3, 4, 1), /* array */[
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

eq("File \"bs_array_test.ml\", line 92, characters 5-12", makeMatrixExn(3, 0, 0), /* array */[
      /* int array */[],
      /* int array */[],
      /* int array */[]
    ]);

eq("File \"bs_array_test.ml\", line 93, characters 5-12", makeMatrixExn(0, 3, 1), /* array */[]);

eq("File \"bs_array_test.ml\", line 94, characters 5-12", makeMatrixExn(1, 1, 1), /* array */[/* int array */[1]]);

eq("File \"bs_array_test.ml\", line 95, characters 5-12", Bs_Array.copy(/* array */[]), /* array */[]);

eq("File \"bs_array_test.ml\", line 96, characters 5-12", Bs_Array.map(/* int array */[], (function (prim) {
            return prim + 1 | 0;
          })), /* int array */[]);

eq("File \"bs_array_test.ml\", line 97, characters 5-12", Bs_Array.mapWithIndex(/* int array */[], add), /* int array */[]);

eq("File \"bs_array_test.ml\", line 98, characters 5-12", Bs_Array.mapWithIndex(/* int array */[
          1,
          2,
          3
        ], add), /* int array */[
      1,
      3,
      5
    ]);

eq("File \"bs_array_test.ml\", line 99, characters 5-12", Bs_List.ofArray(/* array */[]), /* [] */0);

eq("File \"bs_array_test.ml\", line 100, characters 5-12", Bs_List.ofArray(/* int array */[1]), /* :: */[
      1,
      /* [] */0
    ]);

eq("File \"bs_array_test.ml\", line 101, characters 5-12", Bs_List.ofArray(/* int array */[
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

eq("File \"bs_array_test.ml\", line 102, characters 5-12", Bs_Array.map(/* int array */[
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

eq("File \"bs_array_test.ml\", line 103, characters 5-12", Bs_List.toArray(/* [] */0), /* array */[]);

eq("File \"bs_array_test.ml\", line 104, characters 5-12", Bs_List.toArray(/* :: */[
          1,
          /* [] */0
        ]), /* int array */[1]);

eq("File \"bs_array_test.ml\", line 105, characters 5-12", Bs_List.toArray(/* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ]), /* int array */[
      1,
      2
    ]);

eq("File \"bs_array_test.ml\", line 106, characters 5-12", Bs_List.toArray(/* :: */[
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

var v$1 = Bs_Array.makeBy(10, (function (i) {
        return i;
      }));

var v0 = Bs_Array.keep(v$1, (function (x) {
        return +(x % 2 === 0);
      }));

var v1 = Bs_Array.keep(v$1, (function (x) {
        return +(x % 3 === 0);
      }));

var v2 = Bs_Array.keepMap(v$1, (function (x) {
        if (x % 2) {
          return /* None */0;
        } else {
          return /* Some */[x + 1 | 0];
        }
      }));

eq("File \"bs_array_test.ml\", line 113, characters 5-12", v0, /* array */[
      0,
      2,
      4,
      6,
      8
    ]);

eq("File \"bs_array_test.ml\", line 114, characters 5-12", v1, /* int array */[
      0,
      3,
      6,
      9
    ]);

eq("File \"bs_array_test.ml\", line 115, characters 5-12", v2, /* array */[
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

eq("File \"bs_array_test.ml\", line 119, characters 5-12", Bs_Array.slice(a$2, 0, 2), /* int array */[
      1,
      2
    ]);

eq("File \"bs_array_test.ml\", line 120, characters 5-12", Bs_Array.slice(a$2, 0, 5), /* array */[
      1,
      2,
      3,
      4,
      5
    ]);

eq("File \"bs_array_test.ml\", line 121, characters 5-12", Bs_Array.slice(a$2, 0, 15), /* array */[
      1,
      2,
      3,
      4,
      5
    ]);

eq("File \"bs_array_test.ml\", line 122, characters 5-12", Bs_Array.slice(a$2, 5, 1), /* int array */[]);

eq("File \"bs_array_test.ml\", line 123, characters 5-12", Bs_Array.slice(a$2, 4, 1), /* int array */[5]);

eq("File \"bs_array_test.ml\", line 124, characters 5-12", Bs_Array.slice(a$2, -1, 1), /* int array */[5]);

eq("File \"bs_array_test.ml\", line 125, characters 5-12", Bs_Array.slice(a$2, -1, 2), /* int array */[5]);

eq("File \"bs_array_test.ml\", line 126, characters 5-12", Bs_Array.slice(a$2, -2, 1), /* int array */[4]);

eq("File \"bs_array_test.ml\", line 127, characters 5-12", Bs_Array.slice(a$2, -2, 2), /* int array */[
      4,
      5
    ]);

eq("File \"bs_array_test.ml\", line 128, characters 5-12", Bs_Array.slice(a$2, -2, 3), /* int array */[
      4,
      5
    ]);

eq("File \"bs_array_test.ml\", line 129, characters 5-12", Bs_Array.slice(a$2, -10, 3), /* int array */[
      1,
      2,
      3
    ]);

eq("File \"bs_array_test.ml\", line 130, characters 5-12", Bs_Array.slice(a$2, -10, 4), /* int array */[
      1,
      2,
      3,
      4
    ]);

eq("File \"bs_array_test.ml\", line 131, characters 5-12", Bs_Array.slice(a$2, -10, 5), /* array */[
      1,
      2,
      3,
      4,
      5
    ]);

eq("File \"bs_array_test.ml\", line 132, characters 5-12", Bs_Array.slice(a$2, -10, 6), /* array */[
      1,
      2,
      3,
      4,
      5
    ]);

eq("File \"bs_array_test.ml\", line 133, characters 5-12", Bs_Array.slice(a$2, 0, 0), /* int array */[]);

eq("File \"bs_array_test.ml\", line 134, characters 5-12", Bs_Array.slice(a$2, 0, -1), /* int array */[]);

var a$3 = Bs_Array.makeBy(10, (function (x) {
        return x;
      }));

Bs_Array.fill(a$3, 0, 3, 0);

eq("File \"bs_array_test.ml\", line 139, characters 6-13", Bs_Array.copy(a$3), /* array */[
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

Bs_Array.fill(a$3, 2, 8, 1);

eq("File \"bs_array_test.ml\", line 141, characters 5-12", Bs_Array.copy(a$3), /* array */[
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

Bs_Array.fill(a$3, 8, 1, 9);

eq("File \"bs_array_test.ml\", line 143, characters 5-12", Bs_Array.copy(a$3), /* array */[
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

Bs_Array.fill(a$3, 8, 2, 9);

eq("File \"bs_array_test.ml\", line 145, characters 5-12", Bs_Array.copy(a$3), /* array */[
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

Bs_Array.fill(a$3, 8, 3, 12);

eq("File \"bs_array_test.ml\", line 147, characters 5-12", Bs_Array.copy(a$3), /* array */[
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

Bs_Array.fill(a$3, -2, 3, 11);

eq("File \"bs_array_test.ml\", line 149, characters 5-12", Bs_Array.copy(a$3), /* array */[
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

Bs_Array.fill(a$3, -3, 3, 10);

eq("File \"bs_array_test.ml\", line 151, characters 5-12", Bs_Array.copy(a$3), /* array */[
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

Bs_Array.fill(a$3, -3, 1, 7);

eq("File \"bs_array_test.ml\", line 153, characters 5-12", Bs_Array.copy(a$3), /* array */[
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

Bs_Array.fill(a$3, -13, 1, 7);

eq("File \"bs_array_test.ml\", line 155, characters 5-12", Bs_Array.copy(a$3), /* array */[
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

Bs_Array.fill(a$3, -13, 12, 7);

eq("File \"bs_array_test.ml\", line 157, characters 5-12", Bs_Array.copy(a$3), Bs_Array.make(10, 7));

Bs_Array.fill(a$3, 0, -1, 2);

eq("File \"bs_array_test.ml\", line 159, characters 5-12", Bs_Array.copy(a$3), Bs_Array.make(10, 7));

var a0 = Bs_Array.makeBy(10, (function (x) {
        return x;
      }));

var b0 = Bs_Array.make(10, 3);

Bs_Array.blit(a0, 1, b0, 2, 5);

eq("File \"bs_array_test.ml\", line 165, characters 5-12", Bs_Array.copy(b0), /* array */[
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

Bs_Array.blit(a0, -1, b0, 2, 5);

eq("File \"bs_array_test.ml\", line 168, characters 5-12", Bs_Array.copy(b0), /* array */[
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

Bs_Array.blit(a0, -1, b0, -2, 5);

eq("File \"bs_array_test.ml\", line 171, characters 5-12", Bs_Array.copy(b0), /* array */[
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

Bs_Array.blit(a0, -2, b0, -2, 2);

eq("File \"bs_array_test.ml\", line 174, characters 5-12", Bs_Array.copy(b0), /* array */[
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

Bs_Array.blit(a0, -11, b0, -11, 100);

eq("File \"bs_array_test.ml\", line 177, characters 5-12", Bs_Array.copy(b0), a0);

Bs_Array.blit(a0, -11, b0, -11, 2);

eq("File \"bs_array_test.ml\", line 179, characters 5-12", Bs_Array.copy(b0), a0);

var aa = Bs_Array.makeBy(10, (function (x) {
        return x;
      }));

Bs_Array.blit(aa, -1, aa, 1, 2);

eq("File \"bs_array_test.ml\", line 182, characters 5-12", Bs_Array.copy(aa), /* array */[
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

Bs_Array.blit(aa, -2, aa, 1, 2);

eq("File \"bs_array_test.ml\", line 184, characters 5-12", Bs_Array.copy(aa), /* array */[
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

Bs_Array.blit(aa, -5, aa, 4, 3);

eq("File \"bs_array_test.ml\", line 186, characters 5-12", Bs_Array.copy(aa), /* array */[
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

Bs_Array.blit(aa, 4, aa, 5, 3);

eq("File \"bs_array_test.ml\", line 188, characters 5-12", Bs_Array.copy(aa), /* array */[
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
  var u = Bs_Array.copy(x);
  return eq("File \"bs_array_test.ml\", line 191, characters 5-12", Bs_Array.reverse(x), (Bs_Array.reverseInPlace(u), u));
}

id$1("File \"bs_array_test.ml\", line 196, characters 5-12", /* array */[]);

id$1("File \"bs_array_test.ml\", line 197, characters 5-12", /* int array */[1]);

id$1("File \"bs_array_test.ml\", line 198, characters 5-12", /* int array */[
      1,
      2
    ]);

id$1("File \"bs_array_test.ml\", line 199, characters 5-12", /* int array */[
      1,
      2,
      3
    ]);

id$1("File \"bs_array_test.ml\", line 200, characters 5-12", /* int array */[
      1,
      2,
      3,
      4
    ]);

function every2(xs, ys) {
  var partial_arg = Bs_List.toArray(ys);
  var partial_arg$1 = Bs_List.toArray(xs);
  return (function (param) {
      return Bs_Array.every2(partial_arg$1, partial_arg, param);
    });
}

function some2(xs, ys) {
  var partial_arg = Bs_List.toArray(ys);
  var partial_arg$1 = Bs_List.toArray(xs);
  return (function (param) {
      return Bs_Array.some2(partial_arg$1, partial_arg, param);
    });
}

eq("File \"bs_array_test.ml\", line 210, characters 5-12", every2(/* [] */0, /* :: */[
            1,
            /* [] */0
          ])((function (x, y) {
            return +(x > y);
          })), /* true */1);

eq("File \"bs_array_test.ml\", line 211, characters 5-12", every2(/* :: */[
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

eq("File \"bs_array_test.ml\", line 212, characters 5-12", every2(/* :: */[
            2,
            /* [] */0
          ], /* :: */[
            1,
            /* [] */0
          ])((function (x, y) {
            return +(x > y);
          })), /* true */1);

eq("File \"bs_array_test.ml\", line 213, characters 5-12", every2(/* :: */[
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

eq("File \"bs_array_test.ml\", line 214, characters 5-12", every2(/* :: */[
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

eq("File \"bs_array_test.ml\", line 215, characters 5-12", some2(/* [] */0, /* :: */[
            1,
            /* [] */0
          ])((function (x, y) {
            return +(x > y);
          })), /* false */0);

eq("File \"bs_array_test.ml\", line 216, characters 5-12", some2(/* :: */[
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

eq("File \"bs_array_test.ml\", line 217, characters 5-12", some2(/* :: */[
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

eq("File \"bs_array_test.ml\", line 218, characters 5-12", some2(/* :: */[
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

eq("File \"bs_array_test.ml\", line 219, characters 5-12", some2(/* :: */[
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

eq("File \"bs_array_test.ml\", line 224, characters 5-12", Bs_Array.concat(/* int array */[], /* int array */[
          1,
          2,
          3
        ]), /* int array */[
      1,
      2,
      3
    ]);

eq("File \"bs_array_test.ml\", line 225, characters 5-12", Bs_Array.concat(/* array */[], /* array */[]), /* array */[]);

eq("File \"bs_array_test.ml\", line 226, characters 5-12", Bs_Array.concat(/* int array */[
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

eq("File \"bs_array_test.ml\", line 227, characters 5-12", Bs_Array.concatMany(/* array */[
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

eq("File \"bs_array_test.ml\", line 228, characters 5-12", Bs_Array.concatMany(/* array */[
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

eq("File \"bs_array_test.ml\", line 229, characters 5-12", Bs_Array.concatMany(/* array */[
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

eq("File \"bs_array_test.ml\", line 230, characters 5-12", Bs_Array.concatMany(/* array */[
          /* array */[],
          /* array */[]
        ]), /* array */[]);

Mt.from_pair_suites("File \"bs_array_test.ml\", line 232, characters 23-30", suites[0]);

var A = 0;

var L = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.neq = neq;
exports.A = A;
exports.L = L;
exports.add = add;
exports.addone = addone;
exports.makeMatrixExn = makeMatrixExn;
exports.id = id$1;
/*  Not a pure module */

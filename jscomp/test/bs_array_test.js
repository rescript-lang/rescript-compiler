'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
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
  return eq("File \"bs_array_test.ml\", line 25, characters 5-12", Js_vector.toList(Js_list.toVector(x)), x);
}

eq("File \"bs_array_test.ml\", line 29, characters 5-12", Js_list.toVector(/* :: */[
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

eq("File \"bs_array_test.ml\", line 30, characters 6-13", Js_vector.map((function (x) {
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

eq("File \"bs_array_test.ml\", line 33, characters 5-12", Caml_array.caml_make_vect(5, 3), /* array */[
      3,
      3,
      3,
      3,
      3
    ]);

var a = Js_vector.init(5, (function (i) {
        return i + 1 | 0;
      }));

eq("File \"bs_array_test.ml\", line 35, characters 5-12", (Js_vector.filterInPlace((function (j) {
              return +(j % 2 === 0);
            }), a), a), /* int array */[
      2,
      4
    ]);

var a$1 = Js_vector.init(5, (function (i) {
        return i + 1 | 0;
      }));

eq("File \"bs_array_test.ml\", line 42, characters 5-12", (Js_vector.filterInPlace((function (j) {
              return +(j % 2 !== 0);
            }), a$1), a$1), /* int array */[
      1,
      3,
      5
    ]);

eq("File \"bs_array_test.ml\", line 49, characters 5-12", Js_list.toVector(/* :: */[
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

eq("File \"bs_array_test.ml\", line 51, characters 5-12", Js_list.toVector(/* :: */[
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

var v = Bs_Array.init(3000, (function (i) {
        return i;
      }));

var u = Bs_Array.copy(v);

Bs_Array.shuffleOnly(u);

neq("File \"bs_array_test.ml\", line 63, characters 6-13", u, v);

eq("File \"bs_array_test.ml\", line 65, characters 5-12", Bs_Array.foldLeft(u, 0, add), Bs_Array.foldLeft(v, 0, add));

function addone(x) {
  return x + 1 | 0;
}

eq("File \"bs_array_test.ml\", line 69, characters 5-12", Bs_Array.init(0, (function () {
            return 1;
          })), /* int array */[]);

eq("File \"bs_array_test.ml\", line 70, characters 5-12", Bs_Array.init(3, (function (i) {
            return i;
          })), /* int array */[
      0,
      1,
      2
    ]);

eq("File \"bs_array_test.ml\", line 71, characters 5-12", Bs_Array.makeMatrix(3, 4, 1), /* array */[
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

eq("File \"bs_array_test.ml\", line 74, characters 5-12", Bs_Array.makeMatrix(3, 0, 0), /* array */[
      /* int array */[],
      /* int array */[],
      /* int array */[]
    ]);

eq("File \"bs_array_test.ml\", line 75, characters 5-12", Bs_Array.makeMatrix(0, 3, 1), /* array */[]);

eq("File \"bs_array_test.ml\", line 76, characters 5-12", Bs_Array.makeMatrix(1, 1, 1), /* array */[/* int array */[1]]);

eq("File \"bs_array_test.ml\", line 77, characters 5-12", Bs_Array.copy(/* array */[]), /* array */[]);

eq("File \"bs_array_test.ml\", line 78, characters 5-12", Bs_Array.map(/* int array */[], addone), /* int array */[]);

eq("File \"bs_array_test.ml\", line 79, characters 5-12", Bs_Array.mapi(/* int array */[], add), /* int array */[]);

eq("File \"bs_array_test.ml\", line 80, characters 5-12", Bs_Array.mapi(/* int array */[
          1,
          2,
          3
        ], add), /* int array */[
      1,
      3,
      5
    ]);

eq("File \"bs_array_test.ml\", line 81, characters 5-12", Bs_Array.toList(/* array */[]), /* [] */0);

eq("File \"bs_array_test.ml\", line 82, characters 5-12", Bs_Array.toList(/* int array */[1]), /* :: */[
      1,
      /* [] */0
    ]);

eq("File \"bs_array_test.ml\", line 83, characters 5-12", Bs_Array.toList(/* int array */[
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

eq("File \"bs_array_test.ml\", line 84, characters 5-12", Bs_Array.map(/* int array */[
          1,
          2,
          3
        ], addone), /* int array */[
      2,
      3,
      4
    ]);

eq("File \"bs_array_test.ml\", line 85, characters 5-12", Bs_Array.ofList(/* [] */0), /* array */[]);

eq("File \"bs_array_test.ml\", line 86, characters 5-12", Bs_Array.ofList(/* :: */[
          1,
          /* [] */0
        ]), /* int array */[1]);

eq("File \"bs_array_test.ml\", line 87, characters 5-12", Bs_Array.ofList(/* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ]), /* int array */[
      1,
      2
    ]);

eq("File \"bs_array_test.ml\", line 88, characters 5-12", Bs_Array.ofList(/* :: */[
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

Mt.from_pair_suites("File \"bs_array_test.ml\", line 91, characters 23-30", suites[0]);

var A = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.neq = neq;
exports.id = id;
exports.A = A;
exports.add = add;
exports.addone = addone;
/*  Not a pure module */

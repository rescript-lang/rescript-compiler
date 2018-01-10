'use strict';

var Mt = require("./mt.js");
var Bs_Sort = require("../../lib/js/bs_Sort.js");
var Bs_Array = require("../../lib/js/bs_Array.js");
var Bs_Range = require("../../lib/js/bs_Range.js");
var Array_data_util = require("./array_data_util.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

function cmp(x, y) {
  return x - y | 0;
}

b("File \"bs_sort_test.ml\", line 11, characters 4-11", Bs_Range.forAll(0, 200, (function (i) {
            var v = Array_data_util.randomRange(0, i);
            Bs_Sort.stableSortBy(v, cmp);
            return Bs_Sort.isSorted(v, cmp);
          })));

b("File \"bs_sort_test.ml\", line 17, characters 4-11", Bs_Range.forAll(0, 200, (function (i) {
            var v = Array_data_util.randomRange(0, i);
            v.sort(cmp);
            return Bs_Sort.isSorted(v, cmp);
          })));

b("File \"bs_sort_test.ml\", line 23, characters 4-11", Bs_Sort.isSorted(/* int array */[], cmp));

b("File \"bs_sort_test.ml\", line 26, characters 4-11", Bs_Sort.isSorted(/* int array */[0], cmp));

b("File \"bs_sort_test.ml\", line 29, characters 4-11", Bs_Sort.isSorted(/* int array */[
          0,
          1
        ], cmp));

b("File \"bs_sort_test.ml\", line 31, characters 4-11", 1 - Bs_Sort.isSorted(/* int array */[
          1,
          0
        ], cmp));

var u = Array_data_util.randomRange(0, 1000000);

var u1 = Bs_Array.copy(u);

var u2 = Bs_Array.copy(u);

console.time("bs_sort_test.ml 40");

Bs_Sort.stableSortBy(u, cmp);

console.timeEnd("bs_sort_test.ml 40");

b("File \"bs_sort_test.ml\", line 41, characters 4-11", Bs_Sort.isSorted(u, cmp));

console.time("bs_sort_test.ml 42");

Bs_Sort.stableSortInts(u2);

console.timeEnd("bs_sort_test.ml 42");

b("File \"bs_sort_test.ml\", line 43, characters 4-11", Bs_Sort.isSorted(u2, cmp));

console.time("bs_sort_test.ml 44");

u1.sort(cmp);

console.timeEnd("bs_sort_test.ml 44");

b("File \"bs_sort_test.ml\", line 45, characters 4-11", Bs_Sort.isSorted(u1, cmp));

var u$1 = /* array */[
  /* tuple */[
    1,
    "a"
  ],
  /* tuple */[
    1,
    "b"
  ],
  /* tuple */[
    2,
    "a"
  ]
];

eq("File \"bs_sort_test.ml\", line 50, characters 5-12", (Bs_Sort.stableSortBy(u$1, (function (param, param$1) {
              return param[0] - param$1[0] | 0;
            })), u$1), /* array */[
      /* tuple */[
        1,
        "a"
      ],
      /* tuple */[
        1,
        "b"
      ],
      /* tuple */[
        2,
        "a"
      ]
    ]);

var u$2 = /* array */[
  /* tuple */[
    1,
    "b"
  ],
  /* tuple */[
    1,
    "a"
  ],
  /* tuple */[
    1,
    "b"
  ],
  /* tuple */[
    2,
    "a"
  ]
];

eq("File \"bs_sort_test.ml\", line 56, characters 5-12", (Bs_Sort.stableSortBy(u$2, (function (param, param$1) {
              return param[0] - param$1[0] | 0;
            })), u$2), /* array */[
      /* tuple */[
        1,
        "b"
      ],
      /* tuple */[
        1,
        "a"
      ],
      /* tuple */[
        1,
        "b"
      ],
      /* tuple */[
        2,
        "a"
      ]
    ]);

var u$3 = /* array */[
  /* tuple */[
    1,
    "c"
  ],
  /* tuple */[
    1,
    "b"
  ],
  /* tuple */[
    1,
    "a"
  ],
  /* tuple */[
    1,
    "b"
  ],
  /* tuple */[
    1,
    "c"
  ],
  /* tuple */[
    2,
    "a"
  ]
];

eq("File \"bs_sort_test.ml\", line 62, characters 5-12", (Bs_Sort.stableSortBy(u$3, (function (param, param$1) {
              return param[0] - param$1[0] | 0;
            })), u$3), /* array */[
      /* tuple */[
        1,
        "c"
      ],
      /* tuple */[
        1,
        "b"
      ],
      /* tuple */[
        1,
        "a"
      ],
      /* tuple */[
        1,
        "b"
      ],
      /* tuple */[
        1,
        "c"
      ],
      /* tuple */[
        2,
        "a"
      ]
    ]);

eq("File \"bs_sort_test.ml\", line 71, characters 5-12", Bs_Sort.binSearch(/* array */[
          1,
          2,
          3,
          4,
          33,
          35,
          36
        ], 33, cmp), 4);

eq("File \"bs_sort_test.ml\", line 72, characters 5-12", Bs_Sort.binSearch(/* array */[
          1,
          2,
          3,
          4,
          33,
          35,
          36
        ], 1, cmp), 0);

eq("File \"bs_sort_test.ml\", line 73, characters 5-12", Bs_Sort.binSearch(/* array */[
          1,
          2,
          3,
          4,
          33,
          35,
          36
        ], 2, cmp), 1);

eq("File \"bs_sort_test.ml\", line 74, characters 5-12", Bs_Sort.binSearch(/* array */[
          1,
          2,
          3,
          4,
          33,
          35,
          36
        ], 3, cmp), 2);

eq("File \"bs_sort_test.ml\", line 75, characters 5-12", Bs_Sort.binSearch(/* array */[
          1,
          2,
          3,
          4,
          33,
          35,
          36
        ], 4, cmp), 3);

var aa = Array_data_util.range(0, 1000);

b("File \"bs_sort_test.ml\", line 77, characters 4-11", Bs_Range.forAll(0, 1000, (function (i) {
            return +(Bs_Sort.binSearch(aa, i, cmp) === i);
          })));

var cc = Bs_Array.map(Array_data_util.range(0, 2000), (function (x) {
        return (x << 1);
      }));

eq("File \"bs_sort_test.ml\", line 82, characters 5-12", Bs_Sort.binSearch(cc, 5000, cmp) ^ -1, 2001);

eq("File \"bs_sort_test.ml\", line 83, characters 5-12", Bs_Sort.binSearch(cc, -1, cmp) ^ -1, 0);

eq("File \"bs_sort_test.ml\", line 84, characters 5-12", Bs_Sort.binSearch(cc, 0, cmp), 0);

eq("File \"bs_sort_test.ml\", line 86, characters 5-12", Bs_Sort.binSearch(cc, 1, cmp) ^ -1, 1);

b("File \"bs_sort_test.ml\", line 87, characters 4-11", Bs_Range.forAll(0, 1999, (function (i) {
            return +((Bs_Sort.binSearch(cc, (i << 1) + 1 | 0, cmp) ^ -1) === (i + 1 | 0));
          })));

Mt.from_pair_suites("bs_sort_test.ml", suites[0]);

var I = 0;

var S = 0;

var R = 0;

var A = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.I = I;
exports.S = S;
exports.R = R;
exports.cmp = cmp;
exports.A = A;
/*  Not a pure module */

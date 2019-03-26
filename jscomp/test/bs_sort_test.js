'use strict';

var Mt = require("./mt.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Belt_Range = require("../../lib/js/belt_Range.js");
var Belt_SortArray = require("../../lib/js/belt_SortArray.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Array_data_util = require("./array_data_util.js");
var Belt_SortArrayInt = require("../../lib/js/belt_SortArrayInt.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

function cmp(x, y) {
  return x - y | 0;
}

function unions(xs, ys) {
  var lenX = xs.length;
  var lenY = ys.length;
  var o = new Array(lenX + lenY | 0);
  var v = Belt_SortArray.union(xs, 0, lenX, ys, 0, lenY, o, 0, cmp);
  o.length = v;
  return o;
}

function inters(xs, ys) {
  var lenX = xs.length;
  var lenY = ys.length;
  var o = new Array(lenX);
  var v = Belt_SortArray.intersect(xs, 0, lenX, ys, 0, lenY, o, 0, cmp);
  o.length = v;
  return o;
}

function diffs(xs, ys) {
  var lenX = xs.length;
  var lenY = ys.length;
  var o = new Array(lenX);
  var v = Belt_SortArray.diff(xs, 0, lenX, ys, 0, lenY, o, 0, cmp);
  o.length = v;
  return o;
}

eq("File \"bs_sort_test.ml\", line 32, characters 5-12", unions(Array_data_util.range(1, 10), Array_data_util.range(3, 13)), Array_data_util.range(1, 13));

eq("File \"bs_sort_test.ml\", line 33, characters 5-12", unions(Array_data_util.range(1, 10), Array_data_util.range(9, 13)), Array_data_util.range(1, 13));

eq("File \"bs_sort_test.ml\", line 34, characters 5-12", unions(Array_data_util.range(8, 10), Array_data_util.range(9, 13)), Array_data_util.range(8, 13));

eq("File \"bs_sort_test.ml\", line 35, characters 5-12", unions(Array_data_util.range(0, 2), Array_data_util.range(4, 7)), /* array */[
      0,
      1,
      2,
      4,
      5,
      6,
      7
    ]);

eq("File \"bs_sort_test.ml\", line 39, characters 5-12", inters(Array_data_util.range(1, 10), Array_data_util.range(3, 13)), Array_data_util.range(3, 10));

eq("File \"bs_sort_test.ml\", line 40, characters 5-12", inters(Array_data_util.range(1, 10), Array_data_util.range(9, 13)), Array_data_util.range(9, 10));

eq("File \"bs_sort_test.ml\", line 41, characters 5-12", inters(Array_data_util.range(8, 10), Array_data_util.range(9, 13)), Array_data_util.range(9, 10));

eq("File \"bs_sort_test.ml\", line 42, characters 5-12", inters(Array_data_util.range(0, 2), Array_data_util.range(4, 7)), /* array */[]);

eq("File \"bs_sort_test.ml\", line 45, characters 5-12", diffs(Array_data_util.range(1, 10), Array_data_util.range(3, 13)), Array_data_util.range(1, 2));

eq("File \"bs_sort_test.ml\", line 46, characters 5-12", diffs(Array_data_util.range(1, 10), Array_data_util.range(9, 13)), Array_data_util.range(1, 8));

eq("File \"bs_sort_test.ml\", line 47, characters 5-12", diffs(Array_data_util.range(8, 10), Array_data_util.range(9, 13)), Array_data_util.range(8, 8));

eq("File \"bs_sort_test.ml\", line 48, characters 5-12", diffs(Array_data_util.range(0, 2), Array_data_util.range(4, 7)), /* array */[
      0,
      1,
      2
    ]);

b("File \"bs_sort_test.ml\", line 50, characters 4-11", Belt_Range.every(0, 200, (function (i) {
            var v = Array_data_util.randomRange(0, i);
            Belt_SortArray.stableSortInPlaceBy(v, cmp);
            return Belt_SortArray.isSorted(v, cmp);
          })));

b("File \"bs_sort_test.ml\", line 56, characters 4-11", Belt_Range.every(0, 200, (function (i) {
            var v = Array_data_util.randomRange(0, i);
            Belt_SortArray.stableSortInPlaceBy(v, cmp);
            return Belt_SortArray.isSorted(v, cmp);
          })));

b("File \"bs_sort_test.ml\", line 62, characters 4-11", Belt_SortArray.isSorted(/* array */[], cmp));

b("File \"bs_sort_test.ml\", line 65, characters 4-11", Belt_SortArray.isSorted(/* array */[0], cmp));

b("File \"bs_sort_test.ml\", line 68, characters 4-11", Belt_SortArray.isSorted(/* array */[
          0,
          1
        ], cmp));

b("File \"bs_sort_test.ml\", line 70, characters 4-11", !Belt_SortArray.isSorted(/* array */[
          1,
          0
        ], cmp));

var u = Array_data_util.randomRange(0, 1000000);

var u1 = u.slice(0);

var u2 = u.slice(0);

console.time("test/bs_sort_test.ml 80");

Belt_SortArray.stableSortInPlaceBy(u, cmp);

console.timeEnd("test/bs_sort_test.ml 80");

b("File \"bs_sort_test.ml\", line 81, characters 4-11", Belt_SortArray.isSorted(u, cmp));

console.time("test/bs_sort_test.ml 82");

Belt_SortArrayInt.stableSortInPlace(u2);

console.timeEnd("test/bs_sort_test.ml 82");

b("File \"bs_sort_test.ml\", line 83, characters 4-11", Belt_SortArray.isSorted(u2, cmp));

console.time("test/bs_sort_test.ml 84");

Belt_SortArray.stableSortInPlaceBy(u1, cmp);

console.timeEnd("test/bs_sort_test.ml 84");

b("File \"bs_sort_test.ml\", line 85, characters 4-11", Belt_SortArray.isSorted(u1, cmp));

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

eq("File \"bs_sort_test.ml\", line 90, characters 5-12", Belt_SortArray.stableSortBy(u$1, (function (param, param$1) {
            return param[0] - param$1[0] | 0;
          })), /* array */[
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

eq("File \"bs_sort_test.ml\", line 96, characters 5-12", Belt_SortArray.stableSortBy(u$2, (function (param, param$1) {
            return param[0] - param$1[0] | 0;
          })), /* array */[
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

eq("File \"bs_sort_test.ml\", line 102, characters 5-12", Belt_SortArray.stableSortBy(u$3, (function (param, param$1) {
            return param[0] - param$1[0] | 0;
          })), /* array */[
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

eq("File \"bs_sort_test.ml\", line 111, characters 5-12", Belt_SortArray.binarySearchBy(/* array */[
          1,
          3,
          5,
          7
        ], 4, Caml_primitive.caml_int_compare) ^ -1, 2);

eq("File \"bs_sort_test.ml\", line 112, characters 5-12", Belt_SortArray.binarySearchBy(/* array */[
          1,
          2,
          3,
          4,
          33,
          35,
          36
        ], 33, cmp), 4);

eq("File \"bs_sort_test.ml\", line 113, characters 5-12", Belt_SortArray.binarySearchBy(/* array */[
          1,
          2,
          3,
          4,
          33,
          35,
          36
        ], 1, cmp), 0);

eq("File \"bs_sort_test.ml\", line 114, characters 5-12", Belt_SortArray.binarySearchBy(/* array */[
          1,
          2,
          3,
          4,
          33,
          35,
          36
        ], 2, cmp), 1);

eq("File \"bs_sort_test.ml\", line 115, characters 5-12", Belt_SortArray.binarySearchBy(/* array */[
          1,
          2,
          3,
          4,
          33,
          35,
          36
        ], 3, cmp), 2);

eq("File \"bs_sort_test.ml\", line 116, characters 5-12", Belt_SortArray.binarySearchBy(/* array */[
          1,
          2,
          3,
          4,
          33,
          35,
          36
        ], 4, cmp), 3);

var aa = Array_data_util.range(0, 1000);

b("File \"bs_sort_test.ml\", line 118, characters 4-11", Belt_Range.every(0, 1000, (function (i) {
            return Belt_SortArray.binarySearchBy(aa, i, cmp) === i;
          })));

var cc = Belt_Array.map(Array_data_util.range(0, 2000), (function (x) {
        return (x << 1);
      }));

eq("File \"bs_sort_test.ml\", line 123, characters 5-12", Belt_SortArray.binarySearchBy(cc, 5000, cmp) ^ -1, 2001);

eq("File \"bs_sort_test.ml\", line 124, characters 5-12", Belt_SortArray.binarySearchBy(cc, -1, cmp) ^ -1, 0);

eq("File \"bs_sort_test.ml\", line 125, characters 5-12", Belt_SortArray.binarySearchBy(cc, 0, cmp), 0);

eq("File \"bs_sort_test.ml\", line 127, characters 5-12", Belt_SortArray.binarySearchBy(cc, 1, cmp) ^ -1, 1);

b("File \"bs_sort_test.ml\", line 128, characters 4-11", Belt_Range.every(0, 1999, (function (i) {
            return (Belt_SortArray.binarySearchBy(cc, (i << 1) + 1 | 0, cmp) ^ -1) === (i + 1 | 0);
          })));

function lt(x, y) {
  return x < y;
}

eq("File \"bs_sort_test.ml\", line 135, characters 5-12", Belt_SortArray.strictlySortedLength(/* array */[], lt), 0);

eq("File \"bs_sort_test.ml\", line 136, characters 5-12", Belt_SortArray.strictlySortedLength(/* array */[1], lt), 1);

eq("File \"bs_sort_test.ml\", line 137, characters 5-12", Belt_SortArray.strictlySortedLength(/* array */[
          1,
          1
        ], lt), 1);

eq("File \"bs_sort_test.ml\", line 138, characters 5-12", Belt_SortArray.strictlySortedLength(/* array */[
          1,
          1,
          2
        ], lt), 1);

eq("File \"bs_sort_test.ml\", line 139, characters 5-12", Belt_SortArray.strictlySortedLength(/* array */[
          1,
          2
        ], lt), 2);

eq("File \"bs_sort_test.ml\", line 140, characters 5-12", Belt_SortArray.strictlySortedLength(/* array */[
          1,
          2,
          3,
          4,
          3
        ], lt), 4);

eq("File \"bs_sort_test.ml\", line 141, characters 5-12", Belt_SortArray.strictlySortedLength(/* array */[
          4,
          4,
          3,
          2,
          1
        ], lt), 1);

eq("File \"bs_sort_test.ml\", line 142, characters 5-12", Belt_SortArray.strictlySortedLength(/* array */[
          4,
          3,
          2,
          1
        ], lt), -4);

eq("File \"bs_sort_test.ml\", line 143, characters 5-12", Belt_SortArray.strictlySortedLength(/* array */[
          4,
          3,
          2,
          1,
          0
        ], lt), -5);

Mt.from_pair_suites("Bs_sort_test", suites[0]);

var I = 0;

var S = 0;

var R = 0;

var A = 0;

var SI = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.I = I;
exports.S = S;
exports.R = R;
exports.cmp = cmp;
exports.A = A;
exports.unions = unions;
exports.inters = inters;
exports.diffs = diffs;
exports.SI = SI;
exports.lt = lt;
/*  Not a pure module */

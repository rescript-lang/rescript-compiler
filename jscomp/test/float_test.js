// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let $$Array = require("../../lib/js/array.js");
let Caml_obj = require("../../lib/js/caml_obj.js");
let Mt_global = require("./mt_global.js");
let Pervasives = require("../../lib/js/pervasives.js");
let Primitive_float = require("../../lib/js/primitive_float.js");

let test_id = {
  contents: 0
};

let suites = {
  contents: /* [] */0
};

function eq(loc, x, y) {
  Mt_global.collect_eq(test_id, suites, loc, x, y);
}

function approx(loc, x, y) {
  Mt_global.collect_approx(test_id, suites, loc, x, y);
}

let match = Primitive_float.frexp_float(12.0);

let match$1 = Primitive_float.frexp_float(0);

let match$2 = Primitive_float.frexp_float(-12.0);

let results = $$Array.append([
  [
    Math.log10(2),
    0.301029995663981198
  ],
  [
    Primitive_float.ldexp_float(1, 6),
    64
  ],
  [
    Primitive_float.ldexp_float(1, 5),
    32
  ],
  [
    Primitive_float.ldexp_float(1.e-5, 1024),
    1.79769313486231605e+303
  ],
  [
    Primitive_float.ldexp_float(1, -1024),
    5.56268464626800346e-309
  ],
  [
    Primitive_float.hypot_float(3, 4),
    5
  ],
  [
    Primitive_float.hypot_float(4, 3),
    5
  ],
  [
    Primitive_float.hypot_float(5, 12),
    13
  ],
  [
    Primitive_float.hypot_float(12, 5),
    13
  ],
  [
    Primitive_float.copysign_float(22.3, -1),
    -22.3
  ],
  [
    Primitive_float.copysign_float(22.3, 1),
    22.3
  ],
  [
    Primitive_float.expm1_float(1e-15),
    1.00000000000000067e-15
  ],
  [
    Math.log1p(1e-10),
    9.9999999995000007e-11
  ]
], [
  [
    match$1[0],
    0
  ],
  [
    match$1[1],
    0
  ],
  [
    match[0],
    0.75
  ],
  [
    match[1],
    4
  ],
  [
    match$2[0],
    -0.75
  ],
  [
    match$2[1],
    4
  ]
]);

function from_pairs(ps) {
  return $$Array.to_list($$Array.mapi((i, param) => {
    let b = param[1];
    let a = param[0];
    return [
      "pair " + i,
      param => ({
        TAG: "Approx",
        _0: a,
        _1: b
      })
    ];
  }, ps));
}

let float_compare = Primitive_float.compare;

let generic_compare = Caml_obj.compare;

function float_equal(x, y) {
  return x === y;
}

let generic_equal = Caml_obj.equal;

function float_notequal(x, y) {
  return x !== y;
}

let generic_notequal = Caml_obj.notequal;

function float_lessthan(x, y) {
  return x < y;
}

let generic_lessthan = Caml_obj.lessthan;

function float_greaterthan(x, y) {
  return x > y;
}

let generic_greaterthan = Caml_obj.greaterthan;

function float_lessequal(x, y) {
  return x <= y;
}

let generic_lessequal = Caml_obj.lessequal;

function float_greaterequal(x, y) {
  return x >= y;
}

let generic_greaterequal = Caml_obj.greaterequal;

eq("File \"float_test.res\", line 55, characters 5-12", Pervasives.classify_float(3), "FP_normal");

eq("File \"float_test.res\", line 56, characters 5-12", Primitive_float.modf_float(-3.125), [
  -0.125,
  -3
]);

let match$3 = Primitive_float.modf_float(Number.NaN);

eq("File \"float_test.res\", line 58, characters 4-11", [
  Number.isNaN(match$3[0]),
  Number.isNaN(match$3[1])
], [
  true,
  true
]);

eq("File \"float_test.res\", line 66, characters 5-12", $$Array.map(x => {
  if (x > 0) {
    return 1;
  } else if (x < 0) {
    return -1;
  } else {
    return 0;
  }
}, $$Array.map(param => Primitive_float.compare(param[0], param[1]), [
  [
    1,
    3
  ],
  [
    2,
    1
  ],
  [
    3,
    2
  ]
])), [
  -1,
  1,
  1
]);

eq("File \"float_test.res\", line 75, characters 5-12", Primitive_float.copysign_float(-3, 0), 3);

eq("File \"float_test.res\", line 76, characters 5-12", Primitive_float.copysign_float(3, 0), 3);

eq("File \"float_test.res\", line 77, characters 5-12", Math.log10(10), 1);

eq("File \"float_test.res\", line 78, characters 5-12", Primitive_float.expm1_float(0), 0);

eq("File \"float_test.res\", line 79, characters 5-12", Number("3.0"), 3.0);

approx("File \"float_test.res\", line 80, characters 9-16", Primitive_float.expm1_float(2), 6.38905609893065);

eq("File \"float_test.res\", line 81, characters 5-12", Primitive_float.compare(NaN, NaN), 0);

eq("File \"float_test.res\", line 82, characters 5-12", Caml_obj.compare(NaN, NaN), 0);

eq("File \"float_test.res\", line 83, characters 5-12", Primitive_float.compare(NaN, Pervasives.neg_infinity), -1);

eq("File \"float_test.res\", line 84, characters 5-12", Caml_obj.compare(NaN, Pervasives.neg_infinity), -1);

eq("File \"float_test.res\", line 85, characters 5-12", Primitive_float.compare(Pervasives.neg_infinity, NaN), 1);

eq("File \"float_test.res\", line 86, characters 5-12", Caml_obj.compare(Pervasives.neg_infinity, NaN), 1);

eq("File \"float_test.res\", line 87, characters 5-12", NaN === NaN, false);

eq("File \"float_test.res\", line 88, characters 5-12", Caml_obj.equal(NaN, NaN), false);

eq("File \"float_test.res\", line 89, characters 5-12", 4.2 === NaN, false);

eq("File \"float_test.res\", line 90, characters 5-12", Caml_obj.equal(4.2, NaN), false);

eq("File \"float_test.res\", line 91, characters 5-12", NaN === 4.2, false);

eq("File \"float_test.res\", line 92, characters 5-12", Caml_obj.equal(NaN, 4.2), false);

eq("File \"float_test.res\", line 93, characters 5-12", NaN !== NaN, true);

eq("File \"float_test.res\", line 94, characters 5-12", Caml_obj.notequal(NaN, NaN), true);

eq("File \"float_test.res\", line 95, characters 5-12", 4.2 !== NaN, true);

eq("File \"float_test.res\", line 96, characters 5-12", Caml_obj.notequal(4.2, NaN), true);

eq("File \"float_test.res\", line 97, characters 5-12", NaN !== 4.2, true);

eq("File \"float_test.res\", line 98, characters 5-12", Caml_obj.notequal(NaN, 4.2), true);

eq("File \"float_test.res\", line 99, characters 5-12", NaN < NaN, false);

eq("File \"float_test.res\", line 100, characters 5-12", Caml_obj.lessthan(NaN, NaN), false);

eq("File \"float_test.res\", line 101, characters 5-12", 4.2 < NaN, false);

eq("File \"float_test.res\", line 102, characters 5-12", Caml_obj.lessthan(4.2, NaN), false);

eq("File \"float_test.res\", line 103, characters 5-12", NaN < 4.2, false);

eq("File \"float_test.res\", line 104, characters 5-12", Caml_obj.lessthan(NaN, 4.2), false);

eq("File \"float_test.res\", line 105, characters 5-12", NaN > NaN, false);

eq("File \"float_test.res\", line 106, characters 5-12", Caml_obj.greaterthan(NaN, NaN), false);

eq("File \"float_test.res\", line 107, characters 5-12", 4.2 > NaN, false);

eq("File \"float_test.res\", line 108, characters 5-12", Caml_obj.greaterthan(4.2, NaN), false);

eq("File \"float_test.res\", line 109, characters 5-12", NaN > 4.2, false);

eq("File \"float_test.res\", line 110, characters 5-12", Caml_obj.greaterthan(NaN, 4.2), false);

eq("File \"float_test.res\", line 111, characters 5-12", NaN <= NaN, false);

eq("File \"float_test.res\", line 112, characters 5-12", Caml_obj.lessequal(NaN, NaN), false);

eq("File \"float_test.res\", line 113, characters 5-12", 4.2 <= NaN, false);

eq("File \"float_test.res\", line 114, characters 5-12", Caml_obj.lessequal(4.2, NaN), false);

eq("File \"float_test.res\", line 115, characters 5-12", NaN <= 4.2, false);

eq("File \"float_test.res\", line 116, characters 5-12", Caml_obj.lessequal(NaN, 4.2), false);

eq("File \"float_test.res\", line 117, characters 5-12", NaN >= NaN, false);

eq("File \"float_test.res\", line 118, characters 5-12", Caml_obj.greaterequal(NaN, NaN), false);

eq("File \"float_test.res\", line 119, characters 5-12", 4.2 >= NaN, false);

eq("File \"float_test.res\", line 120, characters 5-12", Caml_obj.greaterequal(4.2, NaN), false);

eq("File \"float_test.res\", line 121, characters 5-12", NaN >= 4.2, false);

eq("File \"float_test.res\", line 122, characters 5-12", Caml_obj.greaterequal(NaN, 4.2), false);

let match$4 = Primitive_float.modf_float(32.3);

let b = match$4[1];

let a = match$4[0];

Mt.from_pair_suites("Float_test", Pervasives.$at({
  hd: [
    "mod_float",
    () => ({
      TAG: "Approx",
      _0: 3.2 % 0.5,
      _1: 0.200000000000000178
    })
  ],
  tl: {
    hd: [
      "modf_float1",
      () => ({
        TAG: "Approx",
        _0: a,
        _1: 0.299999999999997158
      })
    ],
    tl: {
      hd: [
        "modf_float2",
        () => ({
          TAG: "Approx",
          _0: b,
          _1: 32
        })
      ],
      tl: {
        hd: [
          "int_of_float",
          () => ({
            TAG: "Eq",
            _0: 3,
            _1: 3
          })
        ],
        tl: /* [] */0
      }
    }
  }
}, Pervasives.$at(from_pairs(results), suites.contents)));

exports.test_id = test_id;
exports.suites = suites;
exports.eq = eq;
exports.approx = approx;
exports.results = results;
exports.from_pairs = from_pairs;
exports.float_compare = float_compare;
exports.generic_compare = generic_compare;
exports.float_equal = float_equal;
exports.generic_equal = generic_equal;
exports.float_notequal = float_notequal;
exports.generic_notequal = generic_notequal;
exports.float_lessthan = float_lessthan;
exports.generic_lessthan = generic_lessthan;
exports.float_greaterthan = float_greaterthan;
exports.generic_greaterthan = generic_greaterthan;
exports.float_lessequal = float_lessequal;
exports.generic_lessequal = generic_lessequal;
exports.float_greaterequal = float_greaterequal;
exports.generic_greaterequal = generic_greaterequal;
/* results Not a pure module */

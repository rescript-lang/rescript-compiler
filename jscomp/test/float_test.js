'use strict';

var Mt = require("./mt.js");
var Caml = require("../../lib/js/caml.js");
var $$Array = require("../../lib/js/array.js");
var Mt_global = require("./mt_global.js");
var Caml_float = require("../../lib/js/caml_float.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Pervasives = require("../../lib/js/pervasives.js");

var test_id = {
  contents: 0
};

var suites = {
  contents: /* [] */0
};

function eq(loc) {
  return function (param, param$1) {
    return Mt_global.collect_eq(test_id, suites, loc, param, param$1);
  };
}

function approx(loc) {
  return function (param, param$1) {
    return Mt_global.collect_approx(test_id, suites, loc, param, param$1);
  };
}

var epsilon_float = Caml_int64.float_of_bits([
      1018167296,
      0
    ]);

var match = Caml_float.caml_frexp_float(12.0);

var match$1 = Caml_float.caml_frexp_float(0);

var match$2 = Caml_float.caml_frexp_float(-12.0);

var results = $$Array.append([
      [
        Math.log10(2),
        0.301029995663981198
      ],
      [
        Caml_float.caml_ldexp_float(1, 6),
        64
      ],
      [
        Caml_float.caml_ldexp_float(1, 5),
        32
      ],
      [
        Caml_float.caml_ldexp_float(1.e-5, 1024),
        1.79769313486231605e+303
      ],
      [
        Caml_float.caml_ldexp_float(1, -1024),
        5.56268464626800346e-309
      ],
      [
        Caml_float.caml_hypot_float(3, 4),
        5
      ],
      [
        Caml_float.caml_hypot_float(4, 3),
        5
      ],
      [
        Caml_float.caml_hypot_float(5, 12),
        13
      ],
      [
        Caml_float.caml_hypot_float(12, 5),
        13
      ],
      [
        Caml_float.caml_copysign_float(22.3, -1),
        -22.3
      ],
      [
        Caml_float.caml_copysign_float(22.3, 1),
        22.3
      ],
      [
        Caml_float.caml_expm1_float(1e-15),
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
  return $$Array.to_list($$Array.mapi((function (i, param) {
                    var b = param[1];
                    var a = param[0];
                    return [
                            "pair " + i,
                            (function (param) {
                                return {
                                        TAG: /* Approx */5,
                                        _0: a,
                                        _1: b
                                      };
                              })
                          ];
                  }), ps));
}

var float_compare = Caml.caml_float_compare;

Mt_global.collect_eq(test_id, suites, "File \"float_test.ml\", line 47, characters 5-12", Pervasives.classify_float(3), /* FP_normal */0);

Mt_global.collect_eq(test_id, suites, "File \"float_test.ml\", line 48, characters 5-12", Caml_float.caml_modf_float(-3.125), [
      -0.125,
      -3
    ]);

var match$3 = Caml_float.caml_modf_float(Number.NaN);

Mt_global.collect_eq(test_id, suites, "File \"float_test.ml\", line 49, characters 5-12", [
      Number.isNaN(match$3[0]),
      Number.isNaN(match$3[1])
    ], [
      true,
      true
    ]);

Mt_global.collect_eq(test_id, suites, "File \"float_test.ml\", line 52, characters 5-12", $$Array.map((function (x) {
            if (x > 0) {
              return 1;
            } else if (x < 0) {
              return -1;
            } else {
              return 0;
            }
          }), $$Array.map((function (param) {
                return Caml.caml_float_compare(param[0], param[1]);
              }), [
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

Mt_global.collect_eq(test_id, suites, "File \"float_test.ml\", line 56, characters 5-12", Caml_float.caml_copysign_float(-3, 0), 3);

Mt_global.collect_eq(test_id, suites, "File \"float_test.ml\", line 57, characters 5-12", Caml_float.caml_copysign_float(3, 0), 3);

Mt_global.collect_eq(test_id, suites, "File \"float_test.ml\", line 58, characters 5-12", Math.log10(10), 1);

Mt_global.collect_eq(test_id, suites, "File \"float_test.ml\", line 59, characters 5-12", Caml_float.caml_expm1_float(0), 0);

Mt_global.collect_eq(test_id, suites, "File \"float_test.ml\", line 60, characters 5-12", Number("3.0"), 3.0);

Mt_global.collect_approx(test_id, suites, "File \"float_test.ml\", line 61, characters 9-16", Caml_float.caml_expm1_float(2), 6.38905609893065);

var match$4 = Caml_float.caml_modf_float(32.3);

var b = match$4[1];

var a = match$4[0];

Mt.from_pair_suites("Float_test", Pervasives.$at({
          hd: [
            "mod_float",
            (function (param) {
                return {
                        TAG: /* Approx */5,
                        _0: 3.2 % 0.5,
                        _1: 0.200000000000000178
                      };
              })
          ],
          tl: {
            hd: [
              "modf_float1",
              (function (param) {
                  return {
                          TAG: /* Approx */5,
                          _0: a,
                          _1: 0.299999999999997158
                        };
                })
            ],
            tl: {
              hd: [
                "modf_float2",
                (function (param) {
                    return {
                            TAG: /* Approx */5,
                            _0: b,
                            _1: 32
                          };
                  })
              ],
              tl: {
                hd: [
                  "int_of_float",
                  (function (param) {
                      return {
                              TAG: /* Eq */0,
                              _0: 3,
                              _1: 3
                            };
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
exports.epsilon_float = epsilon_float;
exports.results = results;
exports.from_pairs = from_pairs;
exports.float_compare = float_compare;
/* results Not a pure module */

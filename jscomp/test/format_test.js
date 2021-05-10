'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Bytes = require("../../lib/js/bytes.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_format = require("../../lib/js/caml_format.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    tl: suites.contents
  };
  
}

function eq3(loc, a, b, c) {
  eq(loc, a, b);
  eq(loc, b, c);
  return eq(loc, a, c);
}

var M = {};

eq("File \"format_test.ml\", line 28, characters 5-12", 7.875, 7.875);

eq("File \"format_test.ml\", line 31, characters 5-12", -7.875, -7.875);

eq3("File \"format_test.ml\", line 35, characters 6-13", Infinity, Number.POSITIVE_INFINITY, Pervasives.infinity);

eq3("File \"format_test.ml\", line 36, characters 6-13", -Infinity, Number.NEGATIVE_INFINITY, Pervasives.neg_infinity);

eq3("File \"format_test.ml\", line 37, characters 6-13", Pervasives.max_float, 1.79769313486231571e+308, Number.MAX_VALUE);

eq("File \"format_test.ml\", line 38, characters 5-12", Pervasives.classify_float(Infinity), /* FP_infinite */3);

eq("File \"format_test.ml\", line 39, characters 5-12", Pervasives.classify_float(Infinity), /* FP_infinite */3);

eq("File \"format_test.ml\", line 42, characters 5-12", Pervasives.min_float, 2.22507385850720138e-308);

eq("File \"format_test.ml\", line 43, characters 5-12", Pervasives.epsilon_float, 2.22044604925031308e-16);

eq("File \"format_test.ml\", line 44, characters 5-12", 4.94065645841e-324, 5e-324);

eq("File \"format_test.ml\", line 45, characters 5-12", 1.00000000000000022 - 1, Pervasives.epsilon_float);

eq("File \"format_test.ml\", line 47, characters 5-12", 1.11253692925360069e-308 / 2.22507385850720138e-308, 0.5);

eq("File \"format_test.ml\", line 49, characters 5-12", Pervasives.classify_float(1.11253692925360069e-308), /* FP_subnormal */1);

eq("File \"format_test.ml\", line 50, characters 5-12", 1.11253692925360069e-308, 1.11253692925360069e-308);

eq("File \"format_test.ml\", line 52, characters 5-12", 2.22507385850720138e-308, 2.22507385850720138e-308);

eq("File \"format_test.ml\", line 56, characters 5-12", (1 + 255 / 256) * 8, 15.96875);

eq("File \"format_test.ml\", line 59, characters 5-12", (1 + 4095 / 4096) * 8, 15.998046875);

eq("File \"format_test.ml\", line 62, characters 5-12", (1 + 65535 / 65536) * 8, 15.9998779296875);

function f(loc, ls) {
  return List.iter((function (param) {
                return eq(loc, Caml_format.float_of_string(param[0]), param[1]);
              }), ls);
}

f("File \"format_test.ml\", line 75, characters 6-13", {
      hd: [
        "0x3.fp+1",
        7.875
      ],
      tl: {
        hd: [
          " 0x3.fp2",
          15.75
        ],
        tl: {
          hd: [
            " 0x4.fp2",
            19.75
          ],
          tl: /* [] */0
        }
      }
    });

function sl(f) {
  return Caml_format.hexstring_of_float(f, -1, /* '-' */45);
}

function aux_list(loc, ls) {
  return List.iter((function (param) {
                return eq(loc, Caml_format.hexstring_of_float(param[0], -1, /* '-' */45), param[1]);
              }), ls);
}

var literals_0 = [
  7.875,
  "0x1.f8p+2"
];

var literals_1 = {
  hd: [
    0.3,
    "0x1.3333333333333p-2"
  ],
  tl: {
    hd: [
      Pervasives.infinity,
      "infinity"
    ],
    tl: {
      hd: [
        0.4,
        "0x1.999999999999ap-2"
      ],
      tl: {
        hd: [
          0.5,
          "0x1p-1"
        ],
        tl: {
          hd: [
            0.6,
            "0x1.3333333333333p-1"
          ],
          tl: {
            hd: [
              0.7,
              "0x1.6666666666666p-1"
            ],
            tl: {
              hd: [
                0.8,
                "0x1.999999999999ap-1"
              ],
              tl: {
                hd: [
                  0.9,
                  "0x1.ccccccccccccdp-1"
                ],
                tl: /* [] */0
              }
            }
          }
        }
      }
    }
  }
};

var literals = {
  hd: literals_0,
  tl: literals_1
};

aux_list("File \"format_test.ml\", line 109, characters 11-18", literals);

var s = Caml_format.hexstring_of_float(7.875, -1, /* '-' */45);

eq("File \"format_test.ml\", line 112, characters 5-12", Bytes.unsafe_to_string(Bytes.uppercase_ascii(Bytes.unsafe_of_string(s))), "0X1.F8P+2");

function scan_float(loc, s, expect) {
  return eq(loc, Caml_format.float_of_string(s), expect);
}

scan_float("File \"format_test.ml\", line 119, characters 13-20", "0x3f.p1", 126);

scan_float("File \"format_test.ml\", line 120, characters 13-20", "0x1.3333333333333p-2", 0.3);

List.iter((function (param) {
        return scan_float("File \"format_test.ml\", line 122, characters 13-20", param[1], param[0]);
      }), literals);

Mt.from_pair_suites("Format_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.eq3 = eq3;
exports.M = M;
exports.f = f;
exports.sl = sl;
exports.aux_list = aux_list;
exports.literals = literals;
exports.scan_float = scan_float;
/*  Not a pure module */

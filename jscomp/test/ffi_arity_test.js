'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");

function f(v) {
  if (v % 2 === 0) {
    return function (v) {
      return Math.imul(v, v);
    };
  } else {
    return function (v) {
      return v + v | 0;
    };
  }
}

var v = [
    1,
    2,
    3
  ].map(function (a, b) {
      return f(a)(b);
    });

var vv = [
    1,
    2,
    3
  ].map(function (a, b) {
      return a + b | 0;
    });

var hh = [
    "1",
    "2",
    "3"
  ].map(function (x) {
      return parseInt(x);
    });

function u() {
  return 3;
}

var vvv = {
  contents: 0
};

function fff(param) {
  console.log("x");
  console.log("x");
  vvv.contents = vvv.contents + 1 | 0;
  
}

function g() {
  return fff(undefined);
}

function abc(x, y, z) {
  console.log("xx");
  console.log("yy");
  return (x + y | 0) + z | 0;
}

var abc_u = abc;

g();

Mt.from_pair_suites("Ffi_arity_test", {
      hd: [
        "File \"ffi_arity_test.ml\", line 45, characters 4-11",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: v,
                    _1: [
                      0,
                      1,
                      4
                    ]
                  };
          })
      ],
      tl: {
        hd: [
          "File \"ffi_arity_test.ml\", line 46, characters 4-11",
          (function (param) {
              return {
                      TAG: /* Eq */0,
                      _0: vv,
                      _1: [
                        1,
                        3,
                        5
                      ]
                    };
            })
        ],
        tl: {
          hd: [
            "File \"ffi_arity_test.ml\", line 47, characters 4-11",
            (function (param) {
                return {
                        TAG: /* Eq */0,
                        _0: hh,
                        _1: [
                          1,
                          2,
                          3
                        ]
                      };
              })
          ],
          tl: /* [] */0
        }
      }
    });

function bar(fn) {
  return Curry._1(fn, undefined);
}

(Curry._1((function(){console.log("forgiving arity")}), undefined));

exports.f = f;
exports.v = v;
exports.vv = vv;
exports.hh = hh;
exports.u = u;
exports.vvv = vvv;
exports.fff = fff;
exports.g = g;
exports.abc = abc;
exports.abc_u = abc_u;
exports.bar = bar;
/* v Not a pure module */

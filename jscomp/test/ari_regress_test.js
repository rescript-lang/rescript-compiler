'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");

var g = 7;

var h = {
  contents: 0
};

function g1(x, y) {
  var u = x + y | 0;
  h.contents = h.contents + 1 | 0;
  return function (xx, yy) {
    return (xx + yy | 0) + u | 0;
  };
}

var u = 8;

var x = u + 6 | 0;

var partial_arg = g1(3, 4);

function v(param) {
  return partial_arg(6, param);
}

var suites_0 = /* tuple */[
  "curry",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: g,
              _1: 7
            };
    })
];

var suites_1 = /* :: */{
  _0: /* tuple */[
    "curry2",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: 14,
                _1: (Curry._1(v, 1), Curry._1(v, 1))
              };
      })
  ],
  _1: /* :: */{
    _0: /* tuple */[
      "curry3",
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: x,
                  _1: 14
                };
        })
    ],
    _1: /* :: */{
      _0: /* tuple */[
        "File \"ari_regress_test.ml\", line 20, characters 4-11",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: h.contents,
                    _1: 1
                  };
          })
      ],
      _1: /* [] */0
    }
  }
};

var suites = /* :: */{
  _0: suites_0,
  _1: suites_1
};

Mt.from_pair_suites("Ari_regress_test", suites);

/* partial_arg Not a pure module */

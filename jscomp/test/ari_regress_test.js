// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");

function f(x) {
  return function (extra) {
    return x + extra | 0;
  };
}

let g = f(3, 4);

let h = {
  contents: 0
};

function gg(x, y) {
  let u = x + y | 0;
  return function (z) {
    return u + z | 0;
  };
}

function g1(x, y) {
  let u = x + y | 0;
  h.contents = h.contents + 1 | 0;
  return function (xx, yy) {
    return (xx + yy | 0) + u | 0;
  };
}

let x = gg(3, 5, 6);

function v(__x) {
  return g1(3, 4, 6, __x);
}

let suites_0 = [
  "curry",
  (function (param) {
    return {
      TAG: "Eq",
      _0: g,
      _1: 7
    };
  })
];

let suites_1 = {
  hd: [
    "curry2",
    (function (param) {
      return {
        TAG: "Eq",
        _0: 14,
        _1: (v(1), v(1))
      };
    })
  ],
  tl: {
    hd: [
      "curry3",
      (function (param) {
        return {
          TAG: "Eq",
          _0: x,
          _1: 14
        };
      })
    ],
    tl: {
      hd: [
        "File \"ari_regress_test.res\", line 35, characters 5-12",
        (function (param) {
          return {
            TAG: "Eq",
            _0: h.contents,
            _1: 2
          };
        })
      ],
      tl: /* [] */0
    }
  }
};

let suites = {
  hd: suites_0,
  tl: suites_1
};

Mt.from_pair_suites("Ari_regress_test", suites);

/* g Not a pure module */

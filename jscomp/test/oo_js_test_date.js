'use strict';

var Mt = require("./mt.js");

var d = new Date(2016, 2);

function d2(param) {
  return function (param$1, param$2) {
    var prim1 = 2;
    return new Date(param, prim1, param$1);
  };
}

var d3 = d2(2016)(1, undefined);

var suites_0 = [
  "getMonth",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: 2,
              _1: d.getMonth()
            };
    })
];

var suites_1 = {
  hd: [
    "getYear",
    (function (param) {
        return {
                TAG: /* Eq */0,
                _0: [
                  2016,
                  2,
                  1
                ],
                _1: [
                  d3.getFullYear(),
                  d3.getMonth(),
                  d3.getDate()
                ]
              };
      })
  ],
  tl: /* [] */0
};

var suites = {
  hd: suites_0,
  tl: suites_1
};

Mt.from_pair_suites("Oo_js_test_date", suites);

exports.d = d;
exports.d2 = d2;
exports.d3 = d3;
exports.suites = suites;
/* d Not a pure module */

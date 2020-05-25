'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

var d = new Date(2016, 2);

function d2(param) {
  return function (param$1, param$2) {
    var prim = 2;
    return new Date(param, prim, param$1);
  };
}

var d3 = d2(2016)(1, undefined);

var suites_000 = /* tuple */[
  "getMonth",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: 2,
              _1: d.getMonth()
            };
    })
];

var suites_001 = /* :: */{
  _0: /* tuple */[
    "getYear",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: /* tuple */[
                  2016,
                  2,
                  1
                ],
                _1: /* tuple */[
                  d3.getFullYear(),
                  d3.getMonth(),
                  d3.getDate()
                ]
              };
      })
  ],
  _1: /* [] */0
};

var suites = /* :: */{
  _0: suites_000,
  _1: suites_001
};

Mt.from_pair_suites("Oo_js_test_date", suites);

exports.d = d;
exports.d2 = d2;
exports.d3 = d3;
exports.suites = suites;
/* d Not a pure module */

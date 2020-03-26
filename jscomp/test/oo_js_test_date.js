'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

var d = new Date(2016, 2);

function d2(param) {
  return (function (param$1, param$2) {
      var prim = 2;
      return new Date(param, prim, param$1);
    });
}

var d3 = d2(2016)(1, undefined);

var suites_000 = /* tuple */[
  "getMonth",
  (function (param) {
      return /* Eq */Block.__(0, [
                2,
                d.getMonth()
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "getYear",
    (function (param) {
        return /* Eq */Block.__(0, [
                  /* tuple */[
                    2016,
                    2,
                    1
                  ],
                  /* tuple */[
                    d3.getFullYear(),
                    d3.getMonth(),
                    d3.getDate()
                  ]
                ]);
      })
  ],
  /* [] */0
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("Oo_js_test_date", suites);

exports.d = d;
exports.d2 = d2;
exports.d3 = d3;
exports.suites = suites;
/* d Not a pure module */

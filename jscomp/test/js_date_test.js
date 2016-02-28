// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt = require("./mt");

var d = new Date(2016, 2);

function d2(param) {
  return function (param$1, param$2) {
    var prim = param;
    var prim$1 = 2;
    var prim$2 = param$1;
    return new Date(prim, prim$1, prim$2);
  };
}

var d3 = d2(2016)(1, /* () */0);

var suites_000 = /* tuple */[
  "getMonth",
  function () {
    return /* Eq */{
            0: 2,
            1: d.getMonth(),
            length: 2,
            tag: 0
          };
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "getYear",
    function () {
      return /* Eq */{
              0: /* tuple */[
                2016,
                2,
                1
              ],
              1: /* tuple */[
                d3.getFullYear(),
                d3.getMonth(),
                d3.getDate()
              ],
              length: 2,
              tag: 0
            };
    }
  ],
  /* [] */0
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("js_date_test.ml", suites);

exports.d      = d;
exports.d2     = d2;
exports.d3     = d3;
exports.suites = suites;
/* d Not a pure module */

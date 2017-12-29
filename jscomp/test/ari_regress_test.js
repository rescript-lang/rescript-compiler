'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");

var g = 7;

var h = [0];

function g1(x, y) {
  var u = x + y | 0;
  h[0] = h[0] + 1 | 0;
  return (function (xx, yy) {
      return (xx + yy | 0) + u | 0;
    });
}

var u = 8;

var x = (function (z) {
      return u + z | 0;
    })(6);

var partial_arg = g1(3, 4);

function v(param) {
  return partial_arg(6, param);
}

var suites_000 = /* tuple */[
  "curry",
  (function () {
      return /* Eq */Block.__(0, [
                g,
                7
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "curry2",
    (function () {
        return /* Eq */Block.__(0, [
                  14,
                  (Curry._1(v, 1), Curry._1(v, 1))
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "curry3",
      (function () {
          return /* Eq */Block.__(0, [
                    x,
                    14
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "File \"ari_regress_test.ml\", line 20, characters 4-11",
        (function () {
            return /* Eq */Block.__(0, [
                      h[0],
                      1
                    ]);
          })
      ],
      /* [] */0
    ]
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("ari_regress_test.ml", suites);

/* x Not a pure module */

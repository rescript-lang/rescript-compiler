'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Pervasives = require("../../lib/js/pervasives.js");

var g = 7;

var h = /* record */{
  contents: 0
};

function g1(x, y) {
  var u = x + y | 0;
  Pervasives.incr(h);
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
  (function (param) {
      return /* Eq */Block.__(0, [
                g,
                7
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "curry2",
    (function (param) {
        return /* Eq */Block.__(0, [
                  14,
                  (Curry._1(v, 1), Curry._1(v, 1))
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "curry3",
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    14
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "File \"ari_regress_test.ml\", line 20, characters 4-11",
        (function (param) {
            return /* Eq */Block.__(0, [
                      h.contents,
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

Mt.from_pair_suites("Ari_regress_test", suites);

/* x Not a pure module */

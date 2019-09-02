'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");

var g = 7;

var h = /* record */[/* contents */0];

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

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "curry",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: g,
                Arg1: 7
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "curry2",
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: 14,
                  Arg1: (Curry._1(v, 1), Curry._1(v, 1))
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "curry3",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: x,
                    Arg1: 14
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "File \"ari_regress_test.ml\", line 20, characters 4-11",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: h[0],
                      Arg1: 1
                    };
            })
        ],
        Arg1: "[]"
      }
    }
  }
};

Mt.from_pair_suites("Ari_regress_test", suites);

/* x Not a pure module */

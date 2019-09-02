'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");

function f(h, param) {
  console.log(3);
  return Curry.__2(h);
}

Mt.from_pair_suites("Print_alpha_test", /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "File \"print_alpha_test.ml\", line 15, characters 4-11",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: f((function (prim, prim$1) {
                              return prim + prim$1 | 0;
                            }), /* () */0)(1, 2),
                    Arg1: 3
                  };
          })
      ],
      Arg1: "[]"
    });

exports.f = f;
/*  Not a pure module */

'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");

function f(h, _) {
  console.log(3);
  return Curry.__2(h);
}

Mt.from_pair_suites("print_alpha_test.ml", /* :: */[
      /* tuple */[
        "File \"print_alpha_test.ml\", line 15, characters 4-11",
        (function () {
            return /* Eq */Block.__(0, [
                      f((function (prim, prim$1) {
                                return prim + prim$1 | 0;
                              }), /* () */0)(1, 2),
                      3
                    ]);
          })
      ],
      /* [] */0
    ]);

exports.f = f;
/*  Not a pure module */

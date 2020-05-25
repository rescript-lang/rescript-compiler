'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");

function f(h, param) {
  console.log(3);
  return Curry.__2(h);
}

Mt.from_pair_suites("Print_alpha_test", /* :: */{
      _0: /* tuple */[
        "File \"print_alpha_test.ml\", line 15, characters 4-11",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: f((function (prim, prim$1) {
                              return prim + prim$1 | 0;
                            }), undefined)(1, 2),
                    _1: 3
                  };
          })
      ],
      _1: /* [] */0
    });

exports.f = f;
/*  Not a pure module */

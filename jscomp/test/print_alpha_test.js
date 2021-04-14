'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");

function f(h, param) {
  console.log(3);
  return Curry.__2(h);
}

Mt.from_pair_suites("Print_alpha_test", {
      hd: [
        "File \"print_alpha_test.ml\", line 15, characters 4-11",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: f((function (prim0, prim1) {
                              return prim0 + prim1 | 0;
                            }), undefined)(1, 2),
                    _1: 3
                  };
          })
      ],
      tl: /* [] */0
    });

exports.f = f;
/*  Not a pure module */

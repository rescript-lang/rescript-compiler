'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Ffi_js_test = require("./ffi_js_test.js");

var x = ("\x01\x02\x03");

var max = (Math.max);



function $$test(x,y){
  return x + y;
}

;

var regression3 = (Math.max);

var regression4 = (Math.max);

function g(a) {
  var regression = (function(x,y){
   return ""
});
  var regression2 = (Math.max);
  Curry._2(regression, a, Pervasives.failwith);
  Curry._2(regression2, 3, 2);
  regression3(3, 2);
  regression4(3, (function (x) {
          return x;
        }));
  return /* () */0;
}

var max2 = (Math.max);

function umax(a, b) {
  return max2(a, b);
}

function u(h) {
  return max2(3, h);
}

var max3 = (Math.max);

function uu(h) {
  return max2(3, h);
}

var empty = ( Object.keys)(3);

var v = $$test(1, 2);

Mt.from_pair_suites("unsafe_ppx_test.ml", /* :: */[
      /* tuple */[
        "unsafe_max",
        (function () {
            return /* Eq */Block.__(0, [
                      2,
                      max(1, 2)
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "unsafe_test",
          (function () {
              return /* Eq */Block.__(0, [
                        3,
                        v
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "unsafe_max2",
            (function () {
                return /* Eq */Block.__(0, [
                          2,
                          (Math.max)(1, 2)
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "ffi_keys",
              (function () {
                  return /* Eq */Block.__(0, [
                            /* array */["a"],
                            Ffi_js_test.keys(( {a : 3}))
                          ]);
                })
            ],
            /* [] */0
          ]
        ]
      ]
    ]);

exports.x = x;
exports.max = max;
exports.regression3 = regression3;
exports.regression4 = regression4;
exports.g = g;
exports.max2 = max2;
exports.umax = umax;
exports.u = u;
exports.max3 = max3;
exports.uu = uu;
exports.empty = empty;
exports.v = v;
/* x Not a pure module */

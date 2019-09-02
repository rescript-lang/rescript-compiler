'use strict';

var Mt = require("./mt.js");
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

Mt.from_pair_suites("Unsafe_ppx_test", /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "unsafe_max",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: 2,
                    Arg1: max(1, 2)
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "unsafe_test",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: 3,
                      Arg1: v
                    };
            })
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "unsafe_max2",
            (function (param) {
                return /* constructor */{
                        tag: "Eq",
                        Arg0: 2,
                        Arg1: (Math.max)(1, 2)
                      };
              })
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "ffi_keys",
              (function (param) {
                  return /* constructor */{
                          tag: "Eq",
                          Arg0: /* array */["a"],
                          Arg1: Ffi_js_test.keys(( {a : 3}))
                        };
                })
            ],
            Arg1: "[]"
          }
        }
      }
    });

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

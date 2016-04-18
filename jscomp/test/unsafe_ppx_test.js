// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Pervasives = require("../stdlib/pervasives");
var Mt         = require("./mt");
var Caml_curry = require("../runtime/caml_curry");
var Ffi_js     = require("./ffi_js");

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
  Caml_curry.app2(regression, a, Pervasives.failwith);
  Caml_curry.app2(regression2, 3, 2);
  regression3(3, 2);
  return regression4(3, function (x) {
              return x;
            });
}

var max2 = (Math.max);

var umax = max2

function u(param) {
  return max2(3, param);
}

var max3 = (Math.max);

function uu(param) {
  return max2(3, param);
}

var empty = ( Object.keys)(3);

var v = $$test(1, 2);

Mt.from_pair_suites("unsafe_ppx_test.ml", /* :: */[
      /* tuple */[
        "unsafe_max",
        function () {
          return /* Eq */{
                  0: 2,
                  1: max(1, 2),
                  length: 2,
                  tag: 0
                };
        }
      ],
      /* :: */[
        /* tuple */[
          "unsafe_test",
          function () {
            return /* Eq */{
                    0: 3,
                    1: v,
                    length: 2,
                    tag: 0
                  };
          }
        ],
        /* :: */[
          /* tuple */[
            "unsafe_max2",
            function () {
              return /* Eq */{
                      0: 2,
                      1: (Math.max)(1, 2),
                      length: 2,
                      tag: 0
                    };
            }
          ],
          /* :: */[
            /* tuple */[
              "ffi_keys",
              function () {
                return /* Eq */{
                        0: /* array */["a"],
                        1: Ffi_js.keys(( {a : 3})),
                        length: 2,
                        tag: 0
                      };
              }
            ],
            /* [] */0
          ]
        ]
      ]
    ]);

exports.x           = x;
exports.max         = max;
exports.regression3 = regression3;
exports.regression4 = regression4;
exports.g           = g;
exports.max2        = max2;
exports.umax        = umax;
exports.u           = u;
exports.max3        = max3;
exports.uu          = uu;
exports.empty       = empty;
exports.v           = v;
/* x Not a pure module */

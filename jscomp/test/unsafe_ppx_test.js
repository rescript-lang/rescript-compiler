// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt     = require("./mt");
var Ffi_js = require("./ffi_js");

var x = ("\x01\x02\x03");

var max = (Math.max);



function $$test(x,y){
  return x + y;
}

;

var max2 = (Math.max);

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

exports.x     = x;
exports.max   = max;
exports.max2  = max2;
exports.u     = u;
exports.max3  = max3;
exports.uu    = uu;
exports.empty = empty;
exports.v     = v;
/* x Not a pure module */

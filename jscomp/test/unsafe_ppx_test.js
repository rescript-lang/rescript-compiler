// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt = require("./mt");

var x = "\x01\x02\x03";

var max = Math.max;

function u(param) {
  return max(1, param);
}



function $$test(x,y){
  return x + y;
}

;

var empty =  Object.keys(3);

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
                      1: Math.max(1, 2),
                      length: 2,
                      tag: 0
                    };
            }
          ],
          /* [] */0
        ]
      ]
    ]);

exports.x     = x;
exports.max   = max;
exports.u     = u;
exports.empty = empty;
exports.v     = v;
/* x Not a pure module */

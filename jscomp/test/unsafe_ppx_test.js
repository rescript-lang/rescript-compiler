// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt         = require("./mt");
var Caml_curry = require("../runtime/caml_curry");

var x = "\x01\x02\x03";

var max = Math.max;


function $$test(x,y){
  return x + y;
}

;

var v = $$test(1, 2);

Mt.from_pair_suites("unsafe_ppx_test.ml", /* :: */[
      /* tuple */[
        "unsafe_max",
        function () {
          return /* Eq */{
                  0: 2,
                  1: Caml_curry.app2(max, 1, 2),
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
        /* [] */0
      ]
    ]);

exports.x   = x;
exports.max = max;
exports.v   = v;
/* x Not a pure module */

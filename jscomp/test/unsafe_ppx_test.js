// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt         = require("./mt");
var Caml_curry = require("../runtime/caml_curry");

var x = "\x01\x02\x03";

var max = Math.max;

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
      /* [] */0
    ]);

exports.x   = x;
exports.max = max;
/* x Not a pure module */

// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_float = require("../runtime/caml_float");
var Mt         = require("./mt");

var epsilon_float = Caml_float.caml_int64_float_of_bits(4372995238176751616);

Mt.from_pair_suites("float_test.ml", /* :: */[
      /* tuple */[
        "mod_float",
        function () {
          return /* Approx */{
                  0: 3.2 % 0.5,
                  1: 0.200000000000000178,
                  length: 2,
                  tag: 2
                };
        }
      ],
      /* [] */0
    ]);

exports.epsilon_float = epsilon_float;
/*  Not a pure module */

// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Pervasives = require("../stdlib/pervasives");
var Mt         = require("./mt");

function ff(v) {
  return Pervasives.string_of_float(v);
}

function f(v) {
  return "" + v;
}

Mt.from_pair_suites("to_string_test.ml", /* :: */[
      /* tuple */[
        'File "to_string_test.ml", line 7, characters 2-9',
        function () {
          return /* Eq */{
                  0: Pervasives.string_of_float(Pervasives.infinity),
                  1: "inf",
                  length: 2,
                  tag: 0
                };
        }
      ],
      /* :: */[
        /* tuple */[
          'File "to_string_test.ml", line 8, characters 1-8',
          function () {
            return /* Eq */{
                    0: Pervasives.string_of_float(Pervasives.neg_infinity),
                    1: "-inf",
                    length: 2,
                    tag: 0
                  };
          }
        ],
        /* [] */0
      ]
    ]);

exports.ff = ff;
exports.f  = f;
/*  Not a pure module */

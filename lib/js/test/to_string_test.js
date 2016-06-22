// GENERATED CODE BY BUCKLESCRIPT VERSION 0.6.1 , PLEASE EDIT WITH CARE
'use strict';

var Pervasives = require("../pervasives");
var Mt         = require("./mt");
var Block      = require("../block");

var ff = Pervasives.string_of_float

function f(v) {
  return "" + v;
}

Mt.from_pair_suites("to_string_test.ml", /* :: */[
      /* tuple */[
        'File "to_string_test.ml", line 7, characters 2-9',
        function () {
          return /* Eq */Block.__(0, [
                    Pervasives.string_of_float(Pervasives.infinity),
                    "inf"
                  ]);
        }
      ],
      /* :: */[
        /* tuple */[
          'File "to_string_test.ml", line 8, characters 1-8',
          function () {
            return /* Eq */Block.__(0, [
                      Pervasives.string_of_float(Pervasives.neg_infinity),
                      "-inf"
                    ]);
          }
        ],
        /* [] */0
      ]
    ]);

exports.ff = ff;
exports.f  = f;
/*  Not a pure module */

// GENERATED CODE BY BUCKLESCRIPT VERSION 0.4.1 , PLEASE EDIT WITH CARE
'use strict';

var Mt    = require("./mt");
var Block = require("../block");
var Curry = require("../curry");

function f(h, _) {
  console.log(3);
  return function (x, y) {
    return Curry._2(h, x, y);
  };
}

Mt.from_pair_suites("print_alpha_test.ml", /* :: */[
      /* tuple */[
        'File "print_alpha_test.ml", line 15, characters 4-11',
        function () {
          return /* Eq */Block.__(0, [
                    f(function (prim, prim$1) {
                            return prim + prim$1 | 0;
                          }, /* () */0)(1, 2),
                    3
                  ]);
        }
      ],
      /* [] */0
    ]);

exports.f = f;
/*  Not a pure module */

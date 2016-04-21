// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt    = require("./mt");
var Curry = require("../runtime/curry");

function f(h, _) {
  var u = 3;
  console.log(u);
  return function (x, y) {
    return Curry._2(h, x, y);
  };
}

Mt.from_pair_suites("print_alpha_test.ml", /* :: */[
      /* tuple */[
        'File "print_alpha_test.ml", line 15, characters 4-11',
        function () {
          return /* Eq */{
                  0: f(function (prim, prim$1) {
                          return prim + prim$1 | 0;
                        }, /* () */0)(1, 2),
                  1: 3,
                  length: 2,
                  tag: 0
                };
        }
      ],
      /* [] */0
    ]);

exports.f = f;
/*  Not a pure module */

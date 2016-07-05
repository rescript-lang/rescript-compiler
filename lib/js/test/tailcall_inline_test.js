// GENERATED CODE BY BUCKLESCRIPT VERSION 0.7.0 , PLEASE EDIT WITH CARE
'use strict';

var Mt         = require("./mt");
var Block      = require("../block");
var Caml_array = require("../caml_array");
var $$Array    = require("../array");

function f() {
  var f$1 = function (_acc, _n) {
    while(true) {
      var n = _n;
      var acc = _acc;
      if (n > 0) {
        _n = n - 1 | 0;
        _acc = acc + n | 0;
        continue ;
        
      }
      else {
        return acc;
      }
    };
  };
  var v = Caml_array.caml_make_vect(10, 0);
  for(var i = 0; i <= 9; ++i){
    v[i] = f$1(0, i);
  }
  return v;
}

var suites_000 = /* tuple */[
  "acc",
  function () {
    return /* Eq */Block.__(0, [
              f(/* () */0),
              /* array */[
                0,
                1,
                3,
                6,
                10,
                15,
                21,
                28,
                36,
                45
              ]
            ]);
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "array_to_list",
    function () {
      return /* Eq */Block.__(0, [
                /* :: */[
                  1,
                  /* :: */[
                    2,
                    /* :: */[
                      3,
                      /* [] */0
                    ]
                  ]
                ],
                $$Array.to_list(/* int array */[
                      1,
                      2,
                      3
                    ])
              ]);
    }
  ],
  /* [] */0
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("tailcall_inline_test.ml", suites);

exports.f      = f;
exports.suites = suites;
/*  Not a pure module */

// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt         = require("./mt");
var Caml_array = require("../runtime/caml_array");
var $$Array    = require("../stdlib/array");

function f() {
  var f$1 = function (_acc, _n) {
    while(true) {
      var n = _n;
      var acc = _acc;
      if (n > 0) {
        _n = n - 1;
        _acc = acc + n | 0;
        continue ;
        
      }
      else {
        return acc;
      }
    };
  };
  var v = Caml_array.caml_make_vect(10, 0);
  for(var i = 0; i<= 9; ++i){
    v[i] = f$1(0, i);
  }
  return v;
}

var suites_000 = /* tuple */[
  "acc",
  function () {
    return /* Eq */{
            0: f(/* () */0),
            1: /* array */[
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
            ],
            length: 2,
            tag: 0
          };
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "array_to_list",
    function () {
      return /* Eq */{
              0: /* :: */[
                1,
                /* :: */[
                  2,
                  /* :: */[
                    3,
                    /* [] */0
                  ]
                ]
              ],
              1: $$Array.to_list(/* int array */[
                    1,
                    2,
                    3
                  ]),
              length: 2,
              tag: 0
            };
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

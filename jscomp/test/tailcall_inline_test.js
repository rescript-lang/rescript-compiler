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
        _acc = acc + n;
        continue ;
        
      }
      else {
        return acc;
      }
    };
  };
  var len = 10;
  var v = Caml_array.caml_make_vect(len, 0);
  for(var i = 0 ,i_finish = len - 1; i<= i_finish; ++i){
    v[i] = f$1(0, i);
  }
  return v;
}

var suites_001 = [
  /* tuple */0,
  "acc",
  function () {
    return [
            /* Eq */0,
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
          ];
  }
];

var suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "array_to_list",
    function () {
      return [
              /* Eq */0,
              [
                /* :: */0,
                1,
                [
                  /* :: */0,
                  2,
                  [
                    /* :: */0,
                    3,
                    /* [] */0
                  ]
                ]
              ],
              $$Array.to_list(/* array */[
                    1,
                    2,
                    3
                  ])
            ];
    }
  ],
  /* [] */0
];

var suites = [
  /* :: */0,
  suites_001,
  suites_002
];

Mt.from_pair_suites("tailcall_inline_test.ml", suites);

exports.f      = f;
exports.suites = suites;
/*  Not a pure module */

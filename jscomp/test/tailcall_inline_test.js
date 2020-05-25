'use strict';

var Mt = require("./mt.js");
var $$Array = require("../../lib/js/array.js");
var Caml_array = require("../../lib/js/caml_array.js");

function f(param) {
  var f$1 = function (_acc, _n) {
    while(true) {
      var n = _n;
      var acc = _acc;
      if (n <= 0) {
        return acc;
      }
      _n = n - 1 | 0;
      _acc = acc + n | 0;
      continue ;
    };
  };
  var v = Caml_array.caml_make_vect(10, 0);
  for(var i = 0; i <= 9; ++i){
    Caml_array.caml_array_set(v, i, f$1(0, i));
  }
  return v;
}

var suites_0 = /* tuple */[
  "acc",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: f(undefined),
              _1: [
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
            };
    })
];

var suites_1 = /* :: */{
  _0: /* tuple */[
    "array_to_list",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: /* :: */{
                  _0: 1,
                  _1: /* :: */{
                    _0: 2,
                    _1: /* :: */{
                      _0: 3,
                      _1: /* [] */0
                    }
                  }
                },
                _1: $$Array.to_list([
                      1,
                      2,
                      3
                    ])
              };
      })
  ],
  _1: /* [] */0
};

var suites = /* :: */{
  _0: suites_0,
  _1: suites_1
};

Mt.from_pair_suites("Tailcall_inline_test", suites);

exports.f = f;
exports.suites = suites;
/*  Not a pure module */

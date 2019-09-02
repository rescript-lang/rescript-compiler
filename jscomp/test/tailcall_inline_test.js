'use strict';

var Mt = require("./mt.js");
var $$Array = require("../../lib/js/array.js");
var Caml_array = require("../../lib/js/caml_array.js");

function f(param) {
  var f$1 = function (_acc, _n) {
    while(true) {
      var n = _n;
      var acc = _acc;
      if (n > 0) {
        _n = n - 1 | 0;
        _acc = acc + n | 0;
        continue ;
      } else {
        return acc;
      }
    };
  };
  var v = Caml_array.caml_make_vect(10, 0);
  for(var i = 0; i <= 9; ++i){
    Caml_array.caml_array_set(v, i, f$1(0, i));
  }
  return v;
}

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "acc",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: f(/* () */0),
                Arg1: /* array */[
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
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "array_to_list",
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: /* constructor */{
                    tag: "::",
                    Arg0: 1,
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: 2,
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: 3,
                        Arg1: "[]"
                      }
                    }
                  },
                  Arg1: $$Array.to_list(/* array */[
                        1,
                        2,
                        3
                      ])
                };
        })
    ],
    Arg1: "[]"
  }
};

Mt.from_pair_suites("Tailcall_inline_test", suites);

exports.f = f;
exports.suites = suites;
/*  Not a pure module */

'use strict';

var Mt = require("./mt.js");
var Pervasives = require("../../lib/js/pervasives.js");

var ff = Pervasives.string_of_float;

function f(v) {
  return String(v);
}

Mt.from_pair_suites("To_string_test", /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "File \"to_string_test.ml\", line 7, characters 2-9",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: Pervasives.string_of_float(Number.POSITIVE_INFINITY),
                    Arg1: "inf"
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "File \"to_string_test.ml\", line 8, characters 1-8",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: Pervasives.string_of_float(Number.NEGATIVE_INFINITY),
                      Arg1: "-inf"
                    };
            })
        ],
        Arg1: "[]"
      }
    });

exports.ff = ff;
exports.f = f;
/*  Not a pure module */

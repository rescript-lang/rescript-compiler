'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Pervasives = require("../../lib/js/pervasives.js");

var ff = Pervasives.string_of_float;

function f(v) {
  return String(v);
}

Mt.from_pair_suites("to_string_test.ml", /* :: */[
      /* tuple */[
        "File \"to_string_test.ml\", line 7, characters 2-9",
        (function () {
            return /* Eq */Block.__(0, [
                      Pervasives.string_of_float(Number.POSITIVE_INFINITY),
                      "inf"
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "File \"to_string_test.ml\", line 8, characters 1-8",
          (function () {
              return /* Eq */Block.__(0, [
                        Pervasives.string_of_float(Number.NEGATIVE_INFINITY),
                        "-inf"
                      ]);
            })
        ],
        /* [] */0
      ]
    ]);

exports.ff = ff;
exports.f = f;
/*  Not a pure module */

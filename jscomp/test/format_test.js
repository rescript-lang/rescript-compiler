'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Format = require("../../lib/js/format.js");
var Pervasives = require("../../lib/js/pervasives.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function () {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

function u() {
  return Pervasives.$caret$caret(/* Format */[
              /* String_literal */Block.__(11, [
                  "xx ",
                  /* String */Block.__(2, [
                      /* No_padding */0,
                      /* End_of_format */0
                    ])
                ]),
              "xx %s"
            ], /* Format */[
              /* String_literal */Block.__(11, [
                  "yy",
                  /* End_of_format */0
                ]),
              "yy"
            ]);
}

eq("File \"format_test.ml\", line 12, characters 5-12", Curry._1(Format.asprintf(u(/* () */0)), "x"), "xx xyy");

Mt.from_pair_suites("format_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.u = u;
/*  Not a pure module */

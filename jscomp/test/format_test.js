'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Format = require("../../lib/js/format.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_format = require("../../lib/js/caml_format.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
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

function eq3(loc, a, b, c) {
  eq(loc, a, b);
  eq(loc, b, c);
  return eq(loc, a, c);
}

function u(param) {
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

var M = /* module */[];

eq("File \"format_test.ml\", line 26, characters 5-12", Curry._1(Format.asprintf(u(/* () */0)), "x"), "xx xyy");

function f(loc, ls) {
  return List.iter((function (param) {
                return eq(loc, Caml_format.caml_float_of_string(param[0]), param[1]);
              }), ls);
}

Mt.from_pair_suites("Format_test", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.eq3 = eq3;
exports.u = u;
exports.M = M;
exports.f = f;
/*  Not a pure module */

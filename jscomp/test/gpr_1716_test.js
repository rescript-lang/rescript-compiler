'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_obj = require("../../lib/js/caml_obj.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
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

var a = [];

var b = [];

Caml_obj.caml_update_dummy(a, /* record */[/* b */b]);

Caml_obj.caml_update_dummy(b, /* record */[/* a */a]);

function is_inifite(x) {
  return +(x[/* b */0][/* a */0] === x);
}

eq("File \"gpr_1716_test.ml\", line 26, characters 6-13", /* true */1, is_inifite(a));

Mt.from_pair_suites("gpr_1716_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.a = a;
exports.b = b;
exports.is_inifite = is_inifite;
/*  Not a pure module */

'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Printexc = require("../../lib/js/printexc.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = [/* contents : [] */0];

var test_id = [/* contents */0];

function eq(loc, x, y) {
  test_id[/* contents */0] = test_id[/* contents */0] + 1 | 0;
  suites[/* contents */0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[/* contents */0])),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[/* contents */0]
  ];
  return /* () */0;
}

var A = Caml_exceptions.create("Gpr_1501_test.A");

var B = Caml_exceptions.create("Gpr_1501_test.B");

eq("File \"gpr_1501_test.ml\", line 15, characters 7-14", "Not_found", Printexc.to_string(Caml_builtin_exceptions.not_found));

eq("File \"gpr_1501_test.ml\", line 16, characters 7-14", "Gpr_1501_test.A", Printexc.to_string(A));

eq("File \"gpr_1501_test.ml\", line 17, characters 7-14", "Gpr_1501_test.B(1)", Printexc.to_string([
          B,
          1
        ]));

Mt.from_pair_suites("Gpr_1501_test", suites[/* contents */0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.A = A;
exports.B = B;
/*  Not a pure module */

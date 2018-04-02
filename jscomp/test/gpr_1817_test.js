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

function f() {
  var x = new Date();
  var y = new Date();
  return /* tuple */[
          Caml_obj.caml_greaterthan(y, x),
          Caml_obj.caml_lessthan(y, x),
          true
        ];
}

var match = f(/* () */0);

var a2 = match[2];

var a1 = match[1];

var a0 = match[0];

console.log(a0, a1);

eq("File \"gpr_1817_test.ml\", line 19, characters 6-13", a2, true);

Mt.from_pair_suites("gpr_1817_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
exports.a0 = a0;
exports.a1 = a1;
exports.a2 = a2;
/* match Not a pure module */

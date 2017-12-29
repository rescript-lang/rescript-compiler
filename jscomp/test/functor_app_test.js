'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Functor_def = require("./functor_def.js");
var Functor_inst = require("./functor_inst.js");

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

var Y0 = Functor_def.Make(/* Functor_inst */[Functor_inst.say]);

var Y1 = Functor_def.Make(/* Functor_inst */[Functor_inst.say]);

eq("File \"functor_app_test.ml\", line 23, characters 6-13", Curry._2(Y0[/* h */0], 1, 2), 4);

eq("File \"functor_app_test.ml\", line 24, characters 6-13", Curry._2(Y1[/* h */0], 2, 3), 6);

var v = Functor_def.$$return(/* () */0);

eq("File \"functor_app_test.ml\", line 29, characters 6-13", v, 2);

Mt.from_pair_suites("functor_app_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.Y0 = Y0;
exports.Y1 = Y1;
exports.v = v;
/* Y0 Not a pure module */

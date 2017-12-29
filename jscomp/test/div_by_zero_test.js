'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");

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

function add(suite) {
  suites[0] = /* :: */[
    suite,
    suites[0]
  ];
  return /* () */0;
}

add(/* tuple */[
      "File \"div_by_zero_test.ml\", line 14, characters 7-14",
      (function () {
          return /* ThrowAny */Block.__(7, [(function () {
                        Caml_int32.div(3, 0);
                        return /* () */0;
                      })]);
        })
    ]);

add(/* tuple */[
      "File \"div_by_zero_test.ml\", line 15, characters 7-14",
      (function () {
          return /* ThrowAny */Block.__(7, [(function () {
                        Caml_int32.mod_(3, 0);
                        return /* () */0;
                      })]);
        })
    ]);

add(/* tuple */[
      "File \"div_by_zero_test.ml\", line 16, characters 7-14",
      (function () {
          return /* ThrowAny */Block.__(7, [(function () {
                        Caml_int32.div(3, 0);
                        return /* () */0;
                      })]);
        })
    ]);

add(/* tuple */[
      "File \"div_by_zero_test.ml\", line 17, characters 7-14",
      (function () {
          return /* ThrowAny */Block.__(7, [(function () {
                        Caml_int32.mod_(3, 0);
                        return /* () */0;
                      })]);
        })
    ]);

add(/* tuple */[
      "File \"div_by_zero_test.ml\", line 18, characters 7-14",
      (function () {
          return /* ThrowAny */Block.__(7, [(function () {
                        Caml_int64.div(/* int64 */[
                              /* hi */0,
                              /* lo */3
                            ], /* int64 */[
                              /* hi */0,
                              /* lo */0
                            ]);
                        return /* () */0;
                      })]);
        })
    ]);

add(/* tuple */[
      "File \"div_by_zero_test.ml\", line 19, characters 7-14",
      (function () {
          return /* ThrowAny */Block.__(7, [(function () {
                        Caml_int64.mod_(/* int64 */[
                              /* hi */0,
                              /* lo */3
                            ], /* int64 */[
                              /* hi */0,
                              /* lo */0
                            ]);
                        return /* () */0;
                      })]);
        })
    ]);

function div(x, y) {
  return Caml_int32.div(x, y) + 3 | 0;
}

Mt.from_pair_suites("div_by_zero_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.add = add;
exports.div = div;
/*  Not a pure module */

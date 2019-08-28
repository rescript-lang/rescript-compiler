'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Pervasives = require("../../lib/js/pervasives.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
  Pervasives.incr(test_id);
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites.contents
  ];
  return /* () */0;
}

function add(suite) {
  suites.contents = /* :: */[
    suite,
    suites.contents
  ];
  return /* () */0;
}

add(/* tuple */[
      "File \"div_by_zero_test.ml\", line 14, characters 7-14",
      (function (param) {
          return /* ThrowAny */Block.__(7, [(function (param) {
                        Caml_int32.div(3, 0);
                        return /* () */0;
                      })]);
        })
    ]);

add(/* tuple */[
      "File \"div_by_zero_test.ml\", line 15, characters 7-14",
      (function (param) {
          return /* ThrowAny */Block.__(7, [(function (param) {
                        Caml_int32.mod_(3, 0);
                        return /* () */0;
                      })]);
        })
    ]);

add(/* tuple */[
      "File \"div_by_zero_test.ml\", line 16, characters 7-14",
      (function (param) {
          return /* ThrowAny */Block.__(7, [(function (param) {
                        Caml_int32.div(3, 0);
                        return /* () */0;
                      })]);
        })
    ]);

add(/* tuple */[
      "File \"div_by_zero_test.ml\", line 17, characters 7-14",
      (function (param) {
          return /* ThrowAny */Block.__(7, [(function (param) {
                        Caml_int32.mod_(3, 0);
                        return /* () */0;
                      })]);
        })
    ]);

add(/* tuple */[
      "File \"div_by_zero_test.ml\", line 18, characters 7-14",
      (function (param) {
          return /* ThrowAny */Block.__(7, [(function (param) {
                        Caml_int64.div(/* int64 */{
                              hi: 0,
                              lo: 3
                            }, /* int64 */{
                              hi: 0,
                              lo: 0
                            });
                        return /* () */0;
                      })]);
        })
    ]);

add(/* tuple */[
      "File \"div_by_zero_test.ml\", line 19, characters 7-14",
      (function (param) {
          return /* ThrowAny */Block.__(7, [(function (param) {
                        Caml_int64.mod_(/* int64 */{
                              hi: 0,
                              lo: 3
                            }, /* int64 */{
                              hi: 0,
                              lo: 0
                            });
                        return /* () */0;
                      })]);
        })
    ]);

function div(x, y) {
  return Caml_int32.div(x, y) + 3 | 0;
}

Mt.from_pair_suites("Div_by_zero_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.add = add;
exports.div = div;
/*  Not a pure module */

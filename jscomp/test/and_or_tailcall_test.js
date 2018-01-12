'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

function f(b, _, _n) {
  while(true) {
    var n = _n;
    if (n > 100000 || !b) {
      return false;
    } else {
      _n = n + 1 | 0;
      continue ;
    }
  };
}

function or_f(b, _, _n) {
  while(true) {
    var n = _n;
    if (n > 100000) {
      return false;
    } else if (b) {
      return true;
    } else {
      _n = n + 1 | 0;
      continue ;
    }
  };
}

var suites_000 = /* tuple */[
  "and_tail",
  (function () {
      return /* Eq */Block.__(0, [
                false,
                f(true, 1, 0)
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "or_tail",
    (function () {
        return /* Eq */Block.__(0, [
                  false,
                  or_f(false, 1, 0)
                ]);
      })
  ],
  /* [] */0
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("and_or_tailcall_test.ml", suites);

exports.f = f;
exports.or_f = or_f;
exports.suites = suites;
/*  Not a pure module */

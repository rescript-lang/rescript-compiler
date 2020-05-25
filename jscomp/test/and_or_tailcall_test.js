'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

function f(b, x, _n) {
  while(true) {
    var n = _n;
    if (n > 100000) {
      return false;
    }
    if (!b) {
      return false;
    }
    _n = n + 1 | 0;
    continue ;
  };
}

function or_f(b, x, _n) {
  while(true) {
    var n = _n;
    if (n > 100000) {
      return false;
    }
    if (b) {
      return true;
    }
    _n = n + 1 | 0;
    continue ;
  };
}

var suites_000 = /* tuple */[
  "and_tail",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: false,
              _1: f(true, 1, 0)
            };
    })
];

var suites_001 = /* :: */{
  _0: /* tuple */[
    "or_tail",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: false,
                _1: or_f(false, 1, 0)
              };
      })
  ],
  _1: /* [] */0
};

var suites = /* :: */{
  _0: suites_000,
  _1: suites_001
};

Mt.from_pair_suites("And_or_tailcall_test", suites);

exports.f = f;
exports.or_f = or_f;
exports.suites = suites;
/*  Not a pure module */

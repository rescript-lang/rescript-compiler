'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");

var v = {
  contents: 3
};

function f(h) {
  v.contents = v.contents + 1 | 0;
  var partial_arg = 3;
  return function (param) {
    return Curry._2(h, partial_arg, param);
  };
}

f(function (prim, prim$1) {
      return prim + prim$1 | 0;
    });

f(function (prim, prim$1) {
      return prim + prim$1 | 0;
    });

var a = v.contents;

var v$1 = {
  contents: 3
};

function f$1(h) {
  v$1.contents = v$1.contents + 1 | 0;
  var partial_arg = 3;
  return function (param) {
    return Curry._2(h, partial_arg, param);
  };
}

f$1(function (prim, prim$1) {
      return prim + prim$1 | 0;
    });

f$1(function (prim, prim$1) {
      return prim + prim$1 | 0;
    });

var b = v$1.contents;

var v$2 = {
  contents: 3
};

function f$2(h) {
  return Curry._2(h, 2, (v$2.contents = v$2.contents + 1 | 0, 3));
}

f$2(function (prim, prim$1) {
      return prim + prim$1 | 0;
    });

f$2(function (prim, prim$1) {
      return prim + prim$1 | 0;
    });

var c = v$2.contents;

var v$3 = {
  contents: 3
};

function f$3(h, g) {
  v$3.contents = v$3.contents + 1 | 0;
  var partial_arg = 9;
  return function (param) {
    return Curry._2(h, partial_arg, param);
  };
}

f$3((function (prim, prim$1) {
        return prim + prim$1 | 0;
      }), 3);

f$3((function (prim, prim$1) {
        return prim + prim$1 | 0;
      }), 3);

var d = v$3.contents;

Mt.from_pair_suites("Pr_regression_test", /* :: */{
      _0: [
        "partial",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: [
                      5,
                      5,
                      5,
                      5
                    ],
                    _1: [
                      a,
                      b,
                      c,
                      d
                    ]
                  };
          })
      ],
      _1: /* [] */0
    });

exports.a = a;
exports.b = b;
exports.c = c;
exports.d = d;
/*  Not a pure module */

'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Pervasives = require("../../lib/js/pervasives.js");

var v = /* record */{
  contents: 3
};

function f(h) {
  Pervasives.incr(v);
  var partial_arg = 3;
  return (function (param) {
      return Curry._2(h, partial_arg, param);
    });
}

f((function (prim, prim$1) {
        return prim + prim$1 | 0;
      }));

f((function (prim, prim$1) {
        return prim + prim$1 | 0;
      }));

var a = v.contents;

var v$1 = /* record */{
  contents: 3
};

function f$1(h) {
  Pervasives.incr(v$1);
  var partial_arg = 3;
  return (function (param) {
      return Curry._2(h, partial_arg, param);
    });
}

f$1((function (prim, prim$1) {
        return prim + prim$1 | 0;
      }));

f$1((function (prim, prim$1) {
        return prim + prim$1 | 0;
      }));

var b = v$1.contents;

var v$2 = /* record */{
  contents: 3
};

function f$2(h) {
  return Curry._2(h, 2, (Pervasives.incr(v$2), 3));
}

f$2((function (prim, prim$1) {
        return prim + prim$1 | 0;
      }));

f$2((function (prim, prim$1) {
        return prim + prim$1 | 0;
      }));

var c = v$2.contents;

var v$3 = /* record */{
  contents: 3
};

function f$3(h, g) {
  Pervasives.incr(v$3);
  var partial_arg = 9;
  return (function (param) {
      return Curry._2(h, partial_arg, param);
    });
}

f$3((function (prim, prim$1) {
        return prim + prim$1 | 0;
      }), 3);

f$3((function (prim, prim$1) {
        return prim + prim$1 | 0;
      }), 3);

var d = v$3.contents;

Mt.from_pair_suites("Pr_regression_test", /* :: */[
      /* tuple */[
        "partial",
        (function (param) {
            return /* Eq */Block.__(0, [
                      /* tuple */[
                        5,
                        5,
                        5,
                        5
                      ],
                      /* tuple */[
                        a,
                        b,
                        c,
                        d
                      ]
                    ]);
          })
      ],
      /* [] */0
    ]);

exports.a = a;
exports.b = b;
exports.c = c;
exports.d = d;
/*  Not a pure module */

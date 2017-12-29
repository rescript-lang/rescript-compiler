'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
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

var called = [0];

function g() {
  var v = [];
  var next = function (i, b) {
    called[0] = called[0] + 1 | 0;
    if (b) {
      Curry._2(v[0], i, /* false */0);
    }
    return i + 1 | 0;
  };
  Caml_obj.caml_update_dummy(v, [next]);
  console.log("" + next(0, /* true */1));
  return /* () */0;
}

g(/* () */0);

var x = [];

var y = [];

Caml_obj.caml_update_dummy(x, /* :: */[
      1,
      y
    ]);

Caml_obj.caml_update_dummy(y, /* :: */[
      2,
      x
    ]);

eq("File \"rec_fun_test.ml\", line 27, characters 6-13", called[0], 2);

Mt.from_pair_suites("rec_fun_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.called = called;
exports.g = g;
exports.x = x;
exports.y = y;
/*  Not a pure module */

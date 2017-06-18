'use strict';

var Mt         = require("./mt.js");
var Block      = require("../../lib/js/block.js");
var Curry      = require("../../lib/js/curry.js");
var Pervasives = require("../../lib/js/pervasives.js");

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

var a = { };

var b = {
  foo: "42"
};

function map(f, x) {
  if (x) {
    return /* Some */[Curry._1(f, x[0])];
  } else {
    return /* None */0;
  }
}

function make(foo) {
  var partial_arg = map(Pervasives.string_of_int, foo);
  return (function () {
      return {
              foo: partial_arg ? partial_arg[0] : undefined
            };
    });
}

var a_ = make(/* None */0)(/* () */0);

var b_ = make(/* Some */[42])(/* () */0);

console.log(Object.keys(a_));

console.log(a, b, a_, b_);

eq("File \"gpr_1409_test.ml\", line 34, characters 6-13", Object.keys(a_).length, 0);

Mt.from_pair_suites("gpr_1409_test.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
exports.a       = a;
exports.b       = b;
exports.map     = map;
exports.make    = make;
exports.a_      = a_;
exports.b_      = b_;
/* a_ Not a pure module */

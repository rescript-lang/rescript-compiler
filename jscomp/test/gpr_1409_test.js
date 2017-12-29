'use strict';

var Mt = require("./mt.js");
var $$Array = require("../../lib/js/array.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Pervasives = require("../../lib/js/pervasives.js");
var String_set = require("./string_set.js");

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
      var tmp = { };
      if (partial_arg) {
        tmp.foo = partial_arg[0];
      }
      return tmp;
    });
}

var a_ = make(/* None */0)(/* () */0);

var b_ = make(/* Some */[42])(/* () */0);

eq("File \"gpr_1409_test.ml\", line 30, characters 6-13", b_.foo, "42");

console.log(Object.keys(a_));

console.log(a, b, a_, b_);

eq("File \"gpr_1409_test.ml\", line 36, characters 6-13", Object.keys(a_).length, 0);

var test2 = {
  hi: 2
};

function test3(_open, xx__hi) {
  console.log("no inlin");
  var tmp = {
    hi: 2
  };
  if (_open) {
    tmp.open = _open[0];
  }
  if (xx__hi) {
    tmp.xx = xx__hi[0];
  }
  return tmp;
}

function test4(_open, xx__hi) {
  console.log("no inlin");
  var tmp = {
    open: _open,
    hi: 2
  };
  if (xx__hi) {
    tmp.xx = xx__hi[0];
  }
  return tmp;
}

function test5(f, x) {
  console.log("no inline");
  var tmp = {
    hi: 2
  };
  var tmp$1 = Curry._1(f, x);
  if (tmp$1) {
    tmp.open = tmp$1[0];
  }
  var tmp$2 = Curry._1(f, x);
  if (tmp$2) {
    tmp.xx = tmp$2[0];
  }
  return tmp;
}

function test6(f, _) {
  console.log("no inline");
  var x = [3];
  var tmp = {
    hi: 2
  };
  var tmp$1 = (x[0] = x[0] + 1 | 0, /* Some */[x[0]]);
  if (tmp$1) {
    tmp.open = tmp$1[0];
  }
  var tmp$2 = f(x);
  if (tmp$2) {
    tmp.xx = tmp$2[0];
  }
  return tmp;
}

function keys(xs, ys) {
  return String_set.equal(String_set.of_list(xs), String_set.of_list($$Array.to_list(ys)));
}

eq("File \"gpr_1409_test.ml\", line 69, characters 6-13", keys(/* :: */[
          "hi",
          /* [] */0
        ], Object.keys(test3(/* None */0, /* None */0))), /* true */1);

eq("File \"gpr_1409_test.ml\", line 71, characters 6-13", keys(/* :: */[
          "hi",
          /* :: */[
            "open",
            /* [] */0
          ]
        ], Object.keys(test3(/* Some */[2], /* None */0))), /* true */1);

eq("File \"gpr_1409_test.ml\", line 73, characters 6-13", keys(/* :: */[
          "hi",
          /* :: */[
            "open",
            /* :: */[
              "xx",
              /* [] */0
            ]
          ]
        ], Object.keys(test3(/* Some */[2], /* Some */[2]))), /* true */1);

Mt.from_pair_suites("gpr_1409_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.a = a;
exports.b = b;
exports.map = map;
exports.make = make;
exports.a_ = a_;
exports.b_ = b_;
exports.test2 = test2;
exports.test3 = test3;
exports.test4 = test4;
exports.test5 = test5;
exports.test6 = test6;
exports.keys = keys;
/* a_ Not a pure module */

'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_primitive = require("../../lib/js/js_primitive.js");

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

function ok(loc, x) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
          return /* Ok */Block.__(4, [x]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

var match = typeof (___undefined_value) === "undefined" ? undefined : (___undefined_value);

var a = match !== undefined ? 2 : 1;

function test() {
  var match = typeof (__DEV__) === "undefined" ? undefined : (__DEV__);
  if (match !== undefined) {
    console.log("dev mode");
    return /* () */0;
  } else {
    console.log("producton mode");
    return /* () */0;
  }
}

function test2() {
  var match = typeof (__filename) === "undefined" ? undefined : (__filename);
  if (match !== undefined) {
    console.log(match);
    return /* () */0;
  } else {
    console.log("non node environment");
    return /* () */0;
  }
}

function test3() {
  if (Js_primitive.undefined_to_opt(typeof (__DEV__) === "undefined" ? undefined : (__DEV__))) {
    return 0;
  } else {
    console.log("production mode");
    return /* () */0;
  }
}

function f(x) {
  return +(x === undefined);
}

ok("File \"undef_regression2_test.ml\", line 44, characters 5-12", +(a > 0));

eq("File \"undef_regression2_test.ml\", line 45, characters 5-12", a, 1);

Mt.from_pair_suites("undef_regression2_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.ok = ok;
exports.a = a;
exports.test = test;
exports.test2 = test2;
exports.test3 = test3;
exports.f = f;
/* match Not a pure module */

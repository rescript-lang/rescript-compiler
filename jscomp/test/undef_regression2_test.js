'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_option = require("../../lib/js/caml_option.js");

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

function ok(loc, x) {
  Pervasives.incr(test_id);
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Ok */Block.__(4, [x]);
        })
    ],
    suites.contents
  ];
  return /* () */0;
}

var match = typeof ___undefined_value === "undefined" ? undefined : ___undefined_value;

var a = match !== undefined ? 2 : 1;

function test(param) {
  var match = typeof __DEV__ === "undefined" ? undefined : __DEV__;
  if (match !== undefined) {
    console.log("dev mode");
    return /* () */0;
  } else {
    console.log("producton mode");
    return /* () */0;
  }
}

function test2(param) {
  var match = typeof __filename === "undefined" ? undefined : __filename;
  if (match !== undefined) {
    console.log(match);
    return /* () */0;
  } else {
    console.log("non node environment");
    return /* () */0;
  }
}

function test3(param) {
  if (Caml_option.undefined_to_opt(typeof __DEV__ === "undefined" ? undefined : __DEV__) === undefined) {
    console.log("production mode");
    return /* () */0;
  } else {
    return 0;
  }
}

function f(x) {
  return x === undefined;
}

ok("File \"undef_regression2_test.ml\", line 44, characters 5-12", a > 0);

eq("File \"undef_regression2_test.ml\", line 45, characters 5-12", a, 1);

Mt.from_pair_suites("Undef_regression2_test", suites.contents);

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

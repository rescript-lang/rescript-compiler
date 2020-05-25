'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */{
    _0: /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    _1: suites.contents
  };
  
}

function ok(loc, x) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */{
    _0: /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  tag: /* Ok */4,
                  _0: x
                };
        })
    ],
    _1: suites.contents
  };
  
}

var match = typeof ___undefined_value === "undefined" ? undefined : ___undefined_value;

var a = match !== undefined ? 2 : 1;

function test(param) {
  var match = typeof __DEV__ === "undefined" ? undefined : __DEV__;
  if (match !== undefined) {
    console.log("dev mode");
  } else {
    console.log("producton mode");
  }
  
}

function test2(param) {
  var f = typeof __filename === "undefined" ? undefined : __filename;
  if (f !== undefined) {
    console.log(f);
  } else {
    console.log("non node environment");
  }
  
}

function test3(param) {
  if (Caml_option.undefined_to_opt(typeof __DEV__ === "undefined" ? undefined : __DEV__) === undefined) {
    console.log("production mode");
    return ;
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

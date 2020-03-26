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
  
}

function ok(loc, x) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Ok */Block.__(4, [x]);
        })
    ],
    suites.contents
  ];
  
}

var match = typeof ___undefined_value === "undefined" ? void 0 : ___undefined_value;

var a = match !== void 0 ? 2 : 1;

function test(param) {
  var match = typeof __DEV__ === "undefined" ? void 0 : __DEV__;
  if (match !== void 0) {
    console.log("dev mode");
    return ;
  } else {
    console.log("producton mode");
    return ;
  }
}

function test2(param) {
  var match = typeof __filename === "undefined" ? void 0 : __filename;
  if (match !== void 0) {
    console.log(match);
    return ;
  } else {
    console.log("non node environment");
    return ;
  }
}

function test3(param) {
  if (Caml_option.undefined_to_opt(typeof __DEV__ === "undefined" ? void 0 : __DEV__) === void 0) {
    console.log("production mode");
    return ;
  }
  
}

function f(x) {
  return x === void 0;
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

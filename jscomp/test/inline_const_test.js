'use strict';

var Mt = require("./mt.js");
var Int64 = require("../../lib/js/int64.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Inline_const = require("./inline_const.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var H = {};

var f = "hello";

var f1 = "a";

var f2 = "中文";

var f3 = "中文";

var f4 = "中文";

eq("File \"inline_const_test.ml\", line 29, characters 5-12", f, "hello");

eq("File \"inline_const_test.ml\", line 30, characters 5-12", f1, "a");

eq("File \"inline_const_test.ml\", line 31, characters 5-12", f2, "中文");

eq("File \"inline_const_test.ml\", line 32, characters 5-12", f3, "中文");

eq("File \"inline_const_test.ml\", line 33, characters 5-12", f4, "中文");

eq("File \"inline_const_test.ml\", line 34, characters 5-12", true, true);

eq("File \"inline_const_test.ml\", line 35, characters 5-12", 1, 1);

eq("File \"inline_const_test.ml\", line 36, characters 5-12", 3e-6, 0.000003);

var h = Caml_int64.add(Caml_int64.add([
          0,
          100
        ], Int64.one), Caml_int64.one);

Mt.from_pair_suites("File \"inline_const_test.ml\", line 43, characters 22-29", suites.contents);

var f5 = true;

var f6 = 1;

var f7 = 3e-6;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.H = H;
exports.f = f;
exports.f1 = f1;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
exports.f5 = f5;
exports.f6 = f6;
exports.f7 = f7;
exports.h = h;
/*  Not a pure module */

'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
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

function f0(x) {
  var old = x.open;
  x.open = old + 1 | 0;
  return x.open;
}

function f1(x) {
  var old = x.in;
  x.in = old + 1 | 0;
  return x.in;
}

function f2(x) {
  var old = x.MAX_LENGTH;
  x.MAX_LENGTH = old + 1 | 0;
  return x.MAX_LENGTH;
}

function f3(x) {
  var old = x.Capital;
  x.Capital = old + 1 | 0;
  return x.Capital;
}

function f4(x) {
  var old = x._open;
  x._open = old + 1 | 0;
  return x._open;
}

function f5(x) {
  var old = x.open;
  x.open = old + 1 | 0;
  return x.open;
}

function f6(x) {
  var old = x["'x"];
  x["'x"] = old + 1 | 0;
  return x["'x"];
}

function f7(x) {
  var old = x._Capital;
  x._Capital = old + 1 | 0;
  return x._Capital;
}

function f8(x) {
  var old = x._MAX;
  x._MAX = old + 1 | 0;
  return x._MAX;
}

function f9(x) {
  var old = x.__;
  x.__ = old + 1 | 0;
  return x.__;
}

function f10(x) {
  var old = x.__x;
  x.__x = old + 1 | 0;
  return x.__x;
}

function f11(x) {
  var old = x._;
  x._ = old + 1 | 0;
  return x._;
}

function f12(x) {
  var old = x.__;
  x.__ = old + 1 | 0;
  return x.__;
}

eq("File \"name_mangle_test.ml\", line 85, characters 7-14", f0(({open:0})), 1);

eq("File \"name_mangle_test.ml\", line 86, characters 7-14", f1(({in:0})), 1);

eq("File \"name_mangle_test.ml\", line 87, characters 7-14", f2(({MAX_LENGTH:0})), 1);

eq("File \"name_mangle_test.ml\", line 88, characters 7-14", f3(({Capital:0})), 1);

eq("File \"name_mangle_test.ml\", line 89, characters 7-14", f4(({_open:0})), 1);

eq("File \"name_mangle_test.ml\", line 90, characters 7-14", f5(({open:0})), 1);

eq("File \"name_mangle_test.ml\", line 91, characters 7-14", f6(({ "'x" :0} )), 1);

eq("File \"name_mangle_test.ml\", line 92, characters 7-14", f7(({_Capital:0})), 1);

eq("File \"name_mangle_test.ml\", line 93, characters 7-14", f8(({_MAX:0})), 1);

eq("File \"name_mangle_test.ml\", line 94, characters 7-14", f9(({__:0})), 1);

eq("File \"name_mangle_test.ml\", line 95, characters 7-14", f10(({__x:0})), 1);

eq("File \"name_mangle_test.ml\", line 96, characters 7-14", f11(({_:0})), 1);

eq("File \"name_mangle_test.ml\", line 97, characters 7-14", f12(({__:0})), 1);

Mt.from_pair_suites("File \"name_mangle_test.ml\", line 101, characters 23-30", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f0 = f0;
exports.f1 = f1;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
exports.f5 = f5;
exports.f6 = f6;
exports.f7 = f7;
exports.f8 = f8;
exports.f9 = f9;
exports.f10 = f10;
exports.f11 = f11;
exports.f12 = f12;
/*  Not a pure module */

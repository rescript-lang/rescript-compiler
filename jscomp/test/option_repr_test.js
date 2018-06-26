'use strict';

var Mt = require("./mt.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Belt_List = require("../../lib/js/belt_List.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, v) {
  return Mt.bool_suites(test_id, suites, loc, v);
}

function f0(x) {
  var match = x[1];
  if (match !== /* None */0 && match[0]) {
    return 1;
  } else {
    return 2;
  }
}

function f1(u) {
  if (u) {
    return 0;
  } else {
    return 1;
  }
}

function f2(x, y, $staropt$star, _) {
  var z = $staropt$star !== /* None */0 ? $staropt$star[0] : 3;
  console.log(x);
  if (y !== /* None */0) {
    return y[0] + z | 0;
  } else {
    return 0;
  }
}

function f3(x) {
  if (x !== /* None */0) {
    return 1;
  } else {
    return 0;
  }
}

function f4(x) {
  if (x !== /* None */0) {
    return x[0] + 1 | 0;
  } else {
    return 0;
  }
}

function f5() {
  return false;
}

function f6() {
  return true;
}

var f10 = [[[[/* None */0]]]];

var f11 = /* Some */[f10];

var randomized = [false];

function create($staropt$star, _) {
  var random = $staropt$star !== /* None */0 ? $staropt$star[0] : randomized[0];
  if (random) {
    return 2;
  } else {
    return 1;
  }
}

var ff = create([false], /* () */0);

function f13($staropt$star, $staropt$star$1, _) {
  var x = $staropt$star !== /* None */0 ? $staropt$star[0] : 3;
  var y = $staropt$star$1 !== /* None */0 ? $staropt$star$1[0] : 4;
  return x + y | 0;
}

var a = f13([2], /* None */0, /* () */0);

function f12(x) {
  return [x];
}

var length_8_id = Belt_List.makeBy(8, (function (x) {
        return x;
      }));

var length_10_id = Belt_List.makeBy(10, (function (x) {
        return x;
      }));

function f13$1() {
  return Caml_obj.caml_equal(Belt_List.take(length_10_id, 8), [/* :: */[
                1,
                /* :: */[
                  2,
                  /* :: */[
                    3,
                    /* [] */0
                  ]
                ]
              ]]);
}

b("File \"option_repr_test.ml\", line 94, characters 4-11", Caml_obj.caml_lessthan(/* None */0, /* Some */[null]));

b("File \"option_repr_test.ml\", line 95, characters 4-11", Caml_obj.caml_lessthan(/* None */0, /* Some */[undefined]));

console.log(6, undefined);

Mt.from_pair_suites("option_repr_test.ml", suites[0]);

var f7 = /* None */0;

var f8 = [/* None */0];

var f9 = [[/* None */0]];

var N = 0;

var none_arg = /* None */0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
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
exports.randomized = randomized;
exports.create = create;
exports.ff = ff;
exports.a = a;
exports.f12 = f12;
exports.N = N;
exports.length_8_id = length_8_id;
exports.length_10_id = length_10_id;
exports.f13 = f13$1;
exports.none_arg = none_arg;
/* ff Not a pure module */

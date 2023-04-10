'use strict';

var Mt = require("./mt.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

function f_01(param) {
  return hi(function (x) {
              if (x === undefined) {
                console.log("x");
                return ;
              }
              
            });
}

function u(x) {
  if (x > 3) {
    return 1;
  } else if (x < 2) {
    return 2;
  } else if (x > 4) {
    return 0;
  } else {
    return 3;
  }
}

function fx(param) {
  
}

function u0(x) {
  return Caml_option.some(x);
}

var u1 = Caml_option.some(undefined);

function u2(x) {
  return Caml_option.some(x);
}

var u3 = Caml_option.some(undefined);

eq("File \"unit_undefined_test.res\", line 41, characters 3-10", Caml_option.some(undefined), Caml_option.some(undefined));

eq("File \"unit_undefined_test.res\", line 42, characters 3-10", u1, Caml_option.some(undefined));

eq("File \"unit_undefined_test.res\", line 43, characters 3-10", Caml_option.some(undefined), Caml_option.some(undefined));

eq("File \"unit_undefined_test.res\", line 44, characters 3-10", u3, Caml_option.some(undefined));

eq("File \"unit_undefined_test.res\", line 45, characters 3-10", undefined, undefined);

Mt.from_pair_suites("unit_undefined_test.res", suites.contents);

var u4;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f_01 = f_01;
exports.u = u;
exports.fx = fx;
exports.u0 = u0;
exports.u1 = u1;
exports.u2 = u2;
exports.u3 = u3;
exports.u4 = u4;
/*  Not a pure module */

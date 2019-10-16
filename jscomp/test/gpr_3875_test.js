'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");

var result = /* record */[/* contents */""];

function log(x) {
  result[0] = x;
  return /* () */0;
}

var Xx = {
  log: log
};

function compilerBug(a, b, c, f) {
  var exit = 0;
  var exit$1 = 0;
  if (a === "x") {
    exit = 2;
  } else {
    exit$1 = 3;
  }
  if (exit$1 === 3) {
    exit = b === "x" ? 2 : 1;
  }
  switch (exit) {
    case 1 :
        if (c) {
          result[0] = "No x, c is true";
          return /* () */0;
        } else {
          result[0] = "No x, c is false";
          return /* () */0;
        }
    case 2 :
        if (Curry._1(f, /* () */0)) {
          result[0] = "Some x, f returns true";
          return /* () */0;
        } else {
          result[0] = "Some x, f returns false";
          return /* () */0;
        }
    
  }
}

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

compilerBug("x", undefined, true, (function (param) {
        return true;
      }));

eq("File \"gpr_3875_test.ml\", line 36, characters 5-12", result[0], "Some x, f returns true");

Mt.from_pair_suites("gpr_3875_test.ml", suites[0]);

exports.result = result;
exports.Xx = Xx;
exports.compilerBug = compilerBug;
exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */

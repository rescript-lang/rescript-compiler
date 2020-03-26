'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");

var result = {
  contents: ""
};

function log(x) {
  result.contents = x;
  
}

var Xx = {
  log: log
};

function compilerBug(a, b, c, f) {
  var exit = 0;
  if (a !== "x") {
    exit = 2;
  }
  if (exit === 2) {
    if (b === void 0) {
      if (c) {
        result.contents = "No x, c is true";
        return ;
      } else {
        result.contents = "No x, c is false";
        return ;
      }
    }
    if (b !== "x") {
      if (c) {
        result.contents = "No x, c is true";
        return ;
      } else {
        result.contents = "No x, c is false";
        return ;
      }
    }
    
  }
  if (Curry._1(f, void 0)) {
    result.contents = "Some x, f returns true";
    return ;
  } else {
    result.contents = "Some x, f returns false";
    return ;
  }
}

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

compilerBug("x", void 0, true, (function (param) {
        return true;
      }));

eq("File \"gpr_3875_test.ml\", line 36, characters 5-12", result.contents, "Some x, f returns true");

Mt.from_pair_suites("gpr_3875_test.ml", suites.contents);

exports.result = result;
exports.Xx = Xx;
exports.compilerBug = compilerBug;
exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */

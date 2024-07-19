// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");

let result = {
  contents: ""
};

function log(x) {
  result.contents = x;
}

let Xx = {
  log: log
};

function compilerBug(a, b, c, f) {
  let exit = 0;
  if (a !== "x") {
    exit = 2;
  }
  if (exit === 2) {
    if (b === undefined) {
      if (c) {
        result.contents = "No x, c is true";
      } else {
        result.contents = "No x, c is false";
      }
      return;
    }
    if (b !== "x") {
      if (c) {
        result.contents = "No x, c is true";
      } else {
        result.contents = "No x, c is false";
      }
      return;
    }
    
  }
  if (f()) {
    result.contents = "Some x, f returns true";
  } else {
    result.contents = "Some x, f returns false";
  }
}

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

compilerBug("x", undefined, true, (function () {
  return true;
}));

eq("File \"gpr_3875_test.res\", line 35, characters 5-12", result.contents, "Some x, f returns true");

Mt.from_pair_suites("gpr_3875_test.res", suites.contents);

exports.result = result;
exports.Xx = Xx;
exports.compilerBug = compilerBug;
exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */

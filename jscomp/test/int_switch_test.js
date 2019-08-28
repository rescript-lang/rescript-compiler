'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

function f(x) {
  var match = Curry._1(x, /* () */0);
  switch (match) {
    case 1 :
        return /* "a" */97;
    case 2 :
        return /* "b" */98;
    case 3 :
        return /* "c" */99;
    default:
      return /* "x" */120;
  }
}

function f22(x) {
  var match = Curry._1(x, /* () */0);
  switch (match) {
    case 1 :
        return /* "a" */97;
    case 2 :
        return /* "b" */98;
    case 3 :
        return /* "c" */99;
    default:
      return /* "x" */120;
  }
}

function f33(x) {
  var match = Curry._1(x, /* () */0);
  switch (match) {
    case /* A */0 :
        return /* "a" */97;
    case /* B */1 :
        return /* "b" */98;
    case /* C */2 :
        return /* "c" */99;
    case /* D */3 :
        return /* "x" */120;
    
  }
}

eq("File \"int_switch_test.ml\", line 35, characters 6-13", f((function (param) {
            return 1;
          })), /* "a" */97);

eq("File \"int_switch_test.ml\", line 36, characters 6-13", f((function (param) {
            return 2;
          })), /* "b" */98);

eq("File \"int_switch_test.ml\", line 37, characters 6-13", f((function (param) {
            return 3;
          })), /* "c" */99);

eq("File \"int_switch_test.ml\", line 38, characters 6-13", f((function (param) {
            return 0;
          })), /* "x" */120);

eq("File \"int_switch_test.ml\", line 39, characters 6-13", f((function (param) {
            return -1;
          })), /* "x" */120);

Mt.from_pair_suites("Int_switch_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.f = f;
exports.f22 = f22;
exports.f33 = f33;
/*  Not a pure module */

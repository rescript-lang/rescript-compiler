// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  Mt.bool_suites(test_id, suites, loc, x);
}

function f(x) {
  let match = x();
  switch (match) {
    case 1 :
      return /* 'a' */97;
    case 2 :
      return /* 'b' */98;
    case 3 :
      return /* 'c' */99;
    default:
      return /* 'x' */120;
  }
}

function f22(x) {
  let match = x();
  switch (match) {
    case 1 :
      return /* 'a' */97;
    case 2 :
      return /* 'b' */98;
    case 3 :
      return /* 'c' */99;
    default:
      return /* 'x' */120;
  }
}

function f33(x) {
  let match = x();
  switch (match) {
    case "A" :
      return /* 'a' */97;
    case "B" :
      return /* 'b' */98;
    case "C" :
      return /* 'c' */99;
    case "D" :
      return /* 'x' */120;
  }
}

eq("File \"int_switch_test.res\", line 32, characters 3-10", f(() => {
  return 1;
}), /* 'a' */97);

eq("File \"int_switch_test.res\", line 33, characters 3-10", f(() => {
  return 2;
}), /* 'b' */98);

eq("File \"int_switch_test.res\", line 34, characters 3-10", f(() => {
  return 3;
}), /* 'c' */99);

eq("File \"int_switch_test.res\", line 35, characters 3-10", f(() => {
  return 0;
}), /* 'x' */120);

eq("File \"int_switch_test.res\", line 36, characters 3-10", f(() => {
  return -1;
}), /* 'x' */120);

Mt.from_pair_suites("Int_switch_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.f = f;
exports.f22 = f22;
exports.f33 = f33;
/*  Not a pure module */

// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Mt from "./mt.mjs";
import * as Primitive_option from "rescript/lib/es6/Primitive_option.js";

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

function make(s, b, i, param) {
  let tmp = {};
  if (s !== undefined) {
    tmp.s = Primitive_option.valFromOption(s);
  }
  if (b !== undefined) {
    tmp.b = Primitive_option.valFromOption(b);
  }
  if (i !== undefined) {
    tmp.i = Primitive_option.valFromOption(i);
  }
  return tmp;
}

let hh = {
  s: "",
  b: false,
  i: 0
};

eq("File \"optional_regression_test.res\", line 16, characters 3-10", Primitive_option.fromUndefined(hh.s), "");

eq("File \"optional_regression_test.res\", line 17, characters 3-10", Primitive_option.fromUndefined(hh.b), false);

eq("File \"optional_regression_test.res\", line 18, characters 3-10", Primitive_option.fromUndefined(hh.i), 0);

console.log(hh);

Mt.from_pair_suites("Optional_regression_test", suites.contents);

export {
  suites,
  test_id,
  eq,
  make,
  hh,
}
/*  Not a pure module */

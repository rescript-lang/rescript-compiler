// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Mt from "./mt.mjs";

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + test_id.contents.toString()),
      () => ({
        TAG: "Eq",
        _0: x,
        _1: y
      })
    ],
    tl: suites.contents
  };
}

function $$(x, y) {
  return x + y | 0;
}

let v = 3;

function $$$plus(x, y) {
  return Math.imul(x, y);
}

let u = 3;

eq("File \"dollar_escape_test.res\", line 20, characters 3-10", v, 3);

eq("File \"dollar_escape_test.res\", line 21, characters 3-10", u, 3);

Mt.from_pair_suites("Dollar_escape_test", suites.contents);

export {
  suites,
  test_id,
  eq,
  $$,
  v,
  $$$plus,
  u,
}
/*  Not a pure module */

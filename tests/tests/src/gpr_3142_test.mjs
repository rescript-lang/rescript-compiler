// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Mt from "./mt.mjs";

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

let _map = {"a":"x","u":"hi","b":"你","c":"我"};

let _revMap = {"x":"a","hi":"u","你":"b","我":"c"};

function tToJs(param) {
  return _map[param];
}

function tFromJs(param) {
  return _revMap[param];
}

eq("File \"gpr_3142_test.res\", line 17, characters 3-10", tToJs("a"), "x");

eq("File \"gpr_3142_test.res\", line 18, characters 3-10", tToJs("u"), "hi");

eq("File \"gpr_3142_test.res\", line 19, characters 3-10", tToJs("b"), "你");

eq("File \"gpr_3142_test.res\", line 20, characters 3-10", tToJs("c"), "我");

eq("File \"gpr_3142_test.res\", line 22, characters 3-10", tFromJs("x"), "a");

eq("File \"gpr_3142_test.res\", line 23, characters 3-10", tFromJs("hi"), "u");

eq("File \"gpr_3142_test.res\", line 24, characters 3-10", tFromJs("你"), "b");

eq("File \"gpr_3142_test.res\", line 25, characters 3-10", tFromJs("我"), "c");

eq("File \"gpr_3142_test.res\", line 26, characters 3-10", tFromJs("xx"), undefined);

Mt.from_pair_suites("Gpr_3142_test", suites.contents);

let v = tToJs;

let u = tFromJs;

export {
  suites,
  test_id,
  eq,
  tToJs,
  tFromJs,
  v,
  u,
}
/*  Not a pure module */

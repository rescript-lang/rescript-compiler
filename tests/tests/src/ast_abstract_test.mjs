// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Mt from "./mt.mjs";
import * as Primitive_util from "rescript/lib/es6/Primitive_util.js";

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

function tToJs(param) {
  return {
    x: param.x,
    y: param.y,
    z: param.z
  };
}

function tFromJs(param) {
  return {
    x: param.x,
    y: param.y,
    z: param.z
  };
}

let v0 = {
  x: 3,
  y: false,
  z: false
};

let v1 = {
  x: 3,
  y: false,
  z: ""
};

let _map = {"a":"a","b":"b","c":"c"};

function xToJs(param) {
  return param;
}

function xFromJs(param) {
  return Primitive_util.raiseWhenNotFound(_map[param]);
}

function idx(v) {
  eq("File \"ast_abstract_test.res\", line 29, characters 18-25", xFromJs(v), v);
}

idx("a");

idx("b");

idx("c");

Mt.from_pair_suites("Ast_abstract_test", suites.contents);

let x0 = "a";

let x1 = "b";

export {
  suites,
  test_id,
  eq,
  tToJs,
  tFromJs,
  v0,
  v1,
  xToJs,
  xFromJs,
  idx,
  x0,
  x1,
}
/*  Not a pure module */

// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Mt from "./mt.mjs";
import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Belt_MapInt from "rescript/lib/es6/Belt_MapInt.js";
import * as Belt_SetInt from "rescript/lib/es6/Belt_SetInt.js";

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

function b(loc, v) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + test_id.contents.toString()),
      () => ({
        TAG: "Ok",
        _0: v
      })
    ],
    tl: suites.contents
  };
}

let mapOfArray = Belt_MapInt.fromArray;

let setOfArray = Belt_SetInt.fromArray;

function emptyMap() {
  
}

let v = Belt_Array.makeByAndShuffle(1000000, i => [
  i,
  i
]);

let u = Belt_MapInt.fromArray(v);

Belt_MapInt.checkInvariantInternal(u);

let firstHalf = Belt_Array.slice(v, 0, 2000);

let xx = Belt_Array.reduce(firstHalf, u, (acc, param) => Belt_MapInt.remove(acc, param[0]));

Belt_MapInt.checkInvariantInternal(u);

Belt_MapInt.checkInvariantInternal(xx);

Mt.from_pair_suites("Bs_map_test", suites.contents);

let M;

let N;

let A;

export {
  suites,
  test_id,
  eq,
  b,
  M,
  N,
  A,
  mapOfArray,
  setOfArray,
  emptyMap,
}
/* v Not a pure module */

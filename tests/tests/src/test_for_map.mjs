// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_MapInt from "rescript/lib/es6/Belt_MapInt.js";

function assertion_test() {
  let m;
  for (let i = 0; i <= 1000000; ++i) {
    m = Belt_MapInt.set(m, i, i);
  }
  for (let i$1 = 0; i$1 <= 1000000; ++i$1) {
    Belt_MapInt.get(m, i$1);
  }
}

let IntMap;

export {
  IntMap,
  assertion_test,
}
/* No side effect */

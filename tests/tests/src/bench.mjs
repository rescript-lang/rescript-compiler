// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Pervasives from "rescript/lib/es6/Pervasives.js";

function map(f, a) {
  let f$1 = x => f(x);
  let l = a.length;
  if (l === 0) {
    return [];
  }
  let r = Belt_Array.make(l, f$1(a[0]));
  for (let i = 1; i < l; ++i) {
    r[i] = f$1(a[i]);
  }
  return r;
}

function init(l, f) {
  let f$1 = x => f(x);
  if (l === 0) {
    return [];
  }
  if (l < 0) {
    return Pervasives.invalid_arg("Array.init");
  }
  let res = Belt_Array.make(l, f$1(0));
  for (let i = 1; i < l; ++i) {
    res[i] = f$1(i);
  }
  return res;
}

function fold_left(f, x, a) {
  let f$1 = (x, y) => f(x, y);
  let r = x;
  for (let i = 0, i_finish = a.length; i < i_finish; ++i) {
    r = f$1(r, a[i]);
  }
  return r;
}

function f2() {
  let arr = init(3000000, i => i);
  let b = map(i => i + i - 1, arr);
  let v = fold_left((prim0, prim1) => prim0 + prim1, 0, b);
  console.log("%f", v);
}

f2();

export {
  map,
  init,
  fold_left,
  f2,
}
/*  Not a pure module */

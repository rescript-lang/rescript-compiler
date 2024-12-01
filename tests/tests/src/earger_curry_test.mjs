// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Mt from "./mt.mjs";
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
  let arr = Belt_Array.init(30000000, i => i);
  let b = Belt_Array.map(arr, i => i + i - 1);
  let v = Belt_Array.reduceReverse(b, 0, (prim0, prim1) => prim0 + prim1);
  console.log(v.toString());
}

f2();

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

let v = {
  contents: 0
};

let all_v = {
  contents: /* [] */0
};

function add5(a0, a1, a2, a3, a4) {
  console.log([
    a0,
    a1,
    a2,
    a3,
    a4
  ]);
  all_v.contents = {
    hd: v.contents,
    tl: all_v.contents
  };
  return (((a0 + a1 | 0) + a2 | 0) + a3 | 0) + a4 | 0;
}

function f(x) {
  return (extra, extra$1) => add5(x, (v.contents = v.contents + 1 | 0, 1), (v.contents = v.contents + 1 | 0, 2), extra, extra$1);
}

function g(x) {
  let u = (a, b) => add5(x, (v.contents = v.contents + 1 | 0, 1), (v.contents = v.contents + 1 | 0, 2), a, b);
  all_v.contents = {
    hd: v.contents,
    tl: all_v.contents
  };
  return u;
}

let a = f(0)(3, 4);

let b = f(0)(3, 5);

let c = g(0)(3, 4);

let d = g(0)(3, 5);

eq("File \"earger_curry_test.res\", line 148, characters 5-12", a, 10);

eq("File \"earger_curry_test.res\", line 149, characters 5-12", b, 11);

eq("File \"earger_curry_test.res\", line 150, characters 5-12", c, 10);

eq("File \"earger_curry_test.res\", line 151, characters 5-12", d, 11);

eq("File \"earger_curry_test.res\", line 152, characters 5-12", all_v.contents, {
  hd: 8,
  tl: {
    hd: 6,
    tl: {
      hd: 6,
      tl: {
        hd: 4,
        tl: {
          hd: 4,
          tl: {
            hd: 2,
            tl: /* [] */0
          }
        }
      }
    }
  }
});

Mt.from_pair_suites("Earger_curry_test", suites.contents);

export {
  map,
  init,
  fold_left,
  f2,
  suites,
  test_id,
  eq,
  v,
  all_v,
  add5,
  f,
  g,
  a,
  b,
  c,
  d,
}
/*  Not a pure module */

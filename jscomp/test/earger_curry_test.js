// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Caml_array = require("../../lib/js/caml_array.js");
let Pervasives = require("../../lib/js/pervasives.js");

function map(f, a) {
  let f$1 = function (x) {
    return f(x);
  };
  let l = a.length;
  if (l === 0) {
    return [];
  }
  let r = Caml_array.make(l, f$1(a[0]));
  for(let i = 1; i < l; ++i){
    r[i] = f$1(a[i]);
  }
  return r;
}

function init(l, f) {
  let f$1 = function (x) {
    return f(x);
  };
  if (l === 0) {
    return [];
  }
  if (l < 0) {
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "Array.init"
          }
        });
  }
  let res = Caml_array.make(l, f$1(0));
  for(let i = 1; i < l; ++i){
    res[i] = f$1(i);
  }
  return res;
}

function fold_left(f, x, a) {
  let f$1 = function (x, y) {
    return f(x, y);
  };
  let r = x;
  for(let i = 0 ,i_finish = a.length; i < i_finish; ++i){
    r = f$1(r, a[i]);
  }
  return r;
}

function f2() {
  let arr = init(30000000, (function (i) {
    return i;
  }));
  let b = map((function (i) {
    return i + i - 1;
  }), arr);
  let v = fold_left((function (prim0, prim1) {
    return prim0 + prim1;
  }), 0, b);
  console.log(Pervasives.string_of_float(v));
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
      loc + (" id " + String(test_id.contents)),
      (function () {
        return {
          TAG: "Eq",
          _0: x,
          _1: y
        };
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
  return function (extra, extra$1) {
    return add5(x, (v.contents = v.contents + 1 | 0, 1), (v.contents = v.contents + 1 | 0, 2), extra, extra$1);
  };
}

function g(x) {
  let u = function (a, b) {
    return add5(x, (v.contents = v.contents + 1 | 0, 1), (v.contents = v.contents + 1 | 0, 2), a, b);
  };
  all_v.contents = {
    hd: v.contents,
    tl: all_v.contents
  };
  return u;
}

let a = f(0, 3, 4);

let b = f(0, 3, 5);

let c = g(0, 3, 4);

let d = g(0, 3, 5);

eq("File \"earger_curry_test.res\", line 141, characters 5-12", a, 10);

eq("File \"earger_curry_test.res\", line 142, characters 5-12", b, 11);

eq("File \"earger_curry_test.res\", line 143, characters 5-12", c, 10);

eq("File \"earger_curry_test.res\", line 144, characters 5-12", d, 11);

eq("File \"earger_curry_test.res\", line 145, characters 5-12", all_v.contents, {
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

exports.map = map;
exports.init = init;
exports.fold_left = fold_left;
exports.f2 = f2;
exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.v = v;
exports.all_v = all_v;
exports.add5 = add5;
exports.f = f;
exports.g = g;
exports.a = a;
exports.b = b;
exports.c = c;
exports.d = d;
/*  Not a pure module */

// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Caml_array = require("../../lib/js/caml_array.js");
let Pervasives = require("../../lib/js/pervasives.js");

function map(f, a) {
  let f$1 = x => f(x);
  let l = a.length;
  if (l === 0) {
    return [];
  }
  let r = Caml_array.make(l, f$1(a[0]));
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
    throw {
      RE_EXN_ID: "Invalid_argument",
      _1: "Array.init",
      Error: new Error()
    };
  }
  let res = Caml_array.make(l, f$1(0));
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
  console.log(Pervasives.string_of_float(v));
}

f2();

exports.map = map;
exports.init = init;
exports.fold_left = fold_left;
exports.f2 = f2;
/*  Not a pure module */



import * as Caml from "./caml.js";
import * as Caml_int64 from "./caml_int64.js";

function pred(n) {
  return Caml_int64.sub(n, Caml_int64.one);
}

function abs(n) {
  if (Caml.i64_ge(n, Caml_int64.zero)) {
    return n;
  } else {
    return Caml_int64.neg(n);
  }
}

function lognot(n) {
  return Caml_int64.xor(n, Caml_int64.neg_one);
}

let compare = Caml_int64.compare;

let equal = Caml_int64.equal;

let zero = Caml_int64.zero;

let one = Caml_int64.one;

let minus_one = Caml_int64.neg_one;

let succ = Caml_int64.succ;

let max_int = Caml_int64.max_int;

let min_int = Caml_int64.min_int;

export {
  zero,
  one,
  minus_one,
  succ,
  pred,
  abs,
  max_int,
  min_int,
  lognot,
  compare,
  equal,
}
/* No side effect */

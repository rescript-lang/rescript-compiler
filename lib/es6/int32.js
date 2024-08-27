

import * as Caml from "./caml.js";

function succ(n) {
  return n + 1 | 0;
}

function pred(n) {
  return n - 1 | 0;
}

function abs(n) {
  if (n >= 0) {
    return n;
  } else {
    return -n | 0;
  }
}

function lognot(n) {
  return n ^ -1;
}

let compare = Caml.int_compare;

function equal(x, y) {
  return x === y;
}

let zero = 0;

let one = 1;

let minus_one = -1;

let max_int = 2147483647;

let min_int = -2147483648;

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

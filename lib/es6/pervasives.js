

import * as Primitive_exceptions from "./primitive_exceptions.js";

function failwith(s) {
  throw {
    RE_EXN_ID: "Failure",
    _1: s,
    Error: new Error()
  };
}

function invalid_arg(s) {
  throw {
    RE_EXN_ID: "Invalid_argument",
    _1: s,
    Error: new Error()
  };
}

let Exit = /* @__PURE__ */Primitive_exceptions.create("Pervasives.Exit");

function abs(x) {
  if (x >= 0) {
    return x;
  } else {
    return -x | 0;
  }
}

function lnot(x) {
  return x ^ -1;
}

let min_int = -2147483648;

function classify_float(x) {
  if (isFinite(x)) {
    if (Math.abs(x) >= 2.22507385850720138e-308) {
      return "FP_normal";
    } else if (x !== 0) {
      return "FP_subnormal";
    } else {
      return "FP_zero";
    }
  } else if (isNaN(x)) {
    return "FP_nan";
  } else {
    return "FP_infinite";
  }
}

function char_of_int(n) {
  if (n < 0 || n > 255) {
    throw {
      RE_EXN_ID: "Invalid_argument",
      _1: "char_of_int",
      Error: new Error()
    };
  }
  return n;
}

function $at(l1, l2) {
  if (l1) {
    return {
      hd: l1.hd,
      tl: $at(l1.tl, l2)
    };
  } else {
    return l2;
  }
}

let max_int = 2147483647;

let infinity = Infinity;

let neg_infinity = -Infinity;

let max_float = 1.79769313486231571e+308;

let min_float = 2.22507385850720138e-308;

let epsilon_float = 2.22044604925031308e-16;

export {
  invalid_arg,
  failwith,
  Exit,
  abs,
  max_int,
  min_int,
  lnot,
  infinity,
  neg_infinity,
  max_float,
  min_float,
  epsilon_float,
  classify_float,
  char_of_int,
  $at,
}
/* No side effect */

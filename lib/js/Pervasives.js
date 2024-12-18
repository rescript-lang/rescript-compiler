'use strict';

let $$Error = require("./Error.js");
let Primitive_object = require("./Primitive_object.js");
let Primitive_exceptions = require("./Primitive_exceptions.js");

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

function string_of_bool(b) {
  if (b) {
    return "true";
  } else {
    return "false";
  }
}

function bool_of_string(param) {
  switch (param) {
    case "false" :
      return false;
    case "true" :
      return true;
    default:
      throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "bool_of_string",
        Error: new Error()
      };
  }
}

function bool_of_string_opt(param) {
  switch (param) {
    case "false" :
      return false;
    case "true" :
      return true;
    default:
      return;
  }
}

function int_of_string_opt(s) {
  let n = Number.parseInt(s);
  if (n === NaN) {
    return;
  } else {
    return n;
  }
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

function assertEqual(a, b) {
  if (!Primitive_object.notequal(a, b)) {
    return;
  }
  throw {
    RE_EXN_ID: "Assert_failure",
    _1: [
      "Pervasives.res",
      596,
      4
    ],
    Error: new Error()
  };
}

let max_int = 2147483647;

let infinity = Infinity;

let neg_infinity = -Infinity;

let max_float = 1.79769313486231571e+308;

let min_float = 2.22507385850720138e-308;

let epsilon_float = 2.22044604925031308e-16;

let panic = $$Error.panic;

exports.failwith = failwith;
exports.invalid_arg = invalid_arg;
exports.Exit = Exit;
exports.abs = abs;
exports.lnot = lnot;
exports.max_int = max_int;
exports.min_int = min_int;
exports.infinity = infinity;
exports.neg_infinity = neg_infinity;
exports.max_float = max_float;
exports.min_float = min_float;
exports.epsilon_float = epsilon_float;
exports.classify_float = classify_float;
exports.char_of_int = char_of_int;
exports.string_of_bool = string_of_bool;
exports.bool_of_string = bool_of_string;
exports.bool_of_string_opt = bool_of_string_opt;
exports.int_of_string_opt = int_of_string_opt;
exports.$at = $at;
exports.panic = panic;
exports.assertEqual = assertEqual;
/* No side effect */

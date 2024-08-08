

import * as Caml from "./caml.js";
import * as Caml_format from "./caml_format.js";
import * as Caml_js_exceptions from "./caml_js_exceptions.js";

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

function to_string(n) {
  return Caml_format.format_int("%d", n);
}

function of_string_opt(s) {
  try {
    return Caml_format.int_of_string(s);
  } catch (raw_exn) {
    let exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      return;
    }
    throw exn;
  }
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
  of_string_opt,
  to_string,
  compare,
  equal,
}
/* No side effect */

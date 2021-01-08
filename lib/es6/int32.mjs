

import * as Caml_format from "./caml_format.mjs";
import * as Caml_primitive from "./caml_primitive.mjs";
import * as Caml_js_exceptions from "./caml_js_exceptions.mjs";

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
  return Caml_format.caml_int32_format("%d", n);
}

function of_string_opt(s) {
  try {
    return Caml_format.caml_int32_of_string(s);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      return ;
    }
    throw exn;
  }
}

var compare = Caml_primitive.caml_int32_compare;

function equal(x, y) {
  return x === y;
}

var zero = 0;

var one = 1;

var minus_one = -1;

var max_int = 2147483647;

var min_int = -2147483648;

export {
  zero ,
  one ,
  minus_one ,
  succ ,
  pred ,
  abs ,
  max_int ,
  min_int ,
  lognot ,
  of_string_opt ,
  to_string ,
  compare ,
  equal ,
  
}
/* No side effect */

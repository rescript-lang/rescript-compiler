

import * as Caml_format from "./caml_format.js";
import * as Caml_primitive from "./caml_primitive.js";
import * as Caml_js_exceptions from "./caml_js_exceptions.js";

function succ(n) {
  return n + 1;
}

function pred(n) {
  return n - 1;
}

function abs(n) {
  if (n >= 0) {
    return n;
  } else {
    return -n;
  }
}

function lognot(n) {
  return n ^ -1;
}

function to_string(n) {
  return Caml_format.caml_nativeint_format("%d", n);
}

function of_string_opt(s) {
  try {
    return Caml_format.caml_nativeint_of_string(s);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      return ;
    }
    throw exn;
  }
}

var compare = Caml_primitive.caml_nativeint_compare;

function equal(x, y) {
  return Caml_primitive.caml_nativeint_compare(x, y) === 0;
}

var zero = 0;

var one = 1;

var minus_one = -1;

var size = 54;

var max_int = 9007199254740991;

var min_int = -9007199254740991;

export {
  zero ,
  one ,
  minus_one ,
  succ ,
  pred ,
  abs ,
  size ,
  max_int ,
  min_int ,
  lognot ,
  of_string_opt ,
  to_string ,
  compare ,
  equal ,
  
}
/* No side effect */

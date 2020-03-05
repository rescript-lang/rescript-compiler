

import * as Caml_int64 from "./caml_int64.js";
import * as Caml_format from "./caml_format.js";
import * as Caml_js_exceptions from "./caml_js_exceptions.js";
import * as Caml_builtin_exceptions from "./caml_builtin_exceptions.js";

function pred(n) {
  return Caml_int64.sub(n, /* int64 */[
              /* hi */0,
              /* lo */1
            ]);
}

function abs(n) {
  if (Caml_int64.ge(n, /* int64 */[
          /* hi */0,
          /* lo */0
        ])) {
    return n;
  } else {
    return Caml_int64.neg(n);
  }
}

function lognot(n) {
  return Caml_int64.xor(n, /* int64 */[
              /* hi */-1,
              /* lo */4294967295
            ]);
}

function of_string_opt(s) {
  try {
    return Caml_format.caml_int64_of_string(s);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Caml_builtin_exceptions.failure) {
      return ;
    } else {
      throw exn;
    }
  }
}

var compare = Caml_int64.compare;

function equal(x, y) {
  return Caml_int64.compare(x, y) === 0;
}

var zero = /* int64 */[
  /* hi */0,
  /* lo */0
];

var one = /* int64 */[
  /* hi */0,
  /* lo */1
];

var minus_one = /* int64 */[
  /* hi */-1,
  /* lo */4294967295
];

var succ = Caml_int64.succ;

var max_int = /* int64 */[
  /* hi */2147483647,
  /* lo */4294967295
];

var min_int = /* int64 */[
  /* hi */-2147483648,
  /* lo */0
];

var to_string = Caml_int64.to_string;

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

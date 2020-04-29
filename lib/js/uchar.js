'use strict';

var Caml_format = require("./caml_format.js");
var Caml_primitive = require("./caml_primitive.js");

function err_not_sv(i) {
  return Caml_format.caml_format_int("%X", i) + " is not an Unicode scalar value";
}

function err_not_latin1(u) {
  return "U+" + (Caml_format.caml_format_int("%04X", u) + " is not a latin1 character");
}

function succ(u) {
  if (u === 55295) {
    return 57344;
  }
  if (u === 1114111) {
    throw {
          ExceptionID: -3,
          _1: "U+10FFFF has no successor",
          Debug: "Invalid_argument"
        };
  }
  return u + 1 | 0;
}

function pred(u) {
  if (u === 57344) {
    return 55295;
  }
  if (u === 0) {
    throw {
          ExceptionID: -3,
          _1: "U+0000 has no predecessor",
          Debug: "Invalid_argument"
        };
  }
  return u - 1 | 0;
}

function is_valid(i) {
  if (0 <= i && i <= 55295) {
    return true;
  } else if (57344 <= i) {
    return i <= 1114111;
  } else {
    return false;
  }
}

function of_int(i) {
  if (is_valid(i)) {
    return i;
  }
  var s = err_not_sv(i);
  throw {
        ExceptionID: -3,
        _1: s,
        Debug: "Invalid_argument"
      };
}

function is_char(u) {
  return u < 256;
}

function of_char(c) {
  return c;
}

function to_char(u) {
  if (u <= 255) {
    return u;
  }
  var s = err_not_latin1(u);
  throw {
        ExceptionID: -3,
        _1: s,
        Debug: "Invalid_argument"
      };
}

function unsafe_to_char(prim) {
  return prim;
}

function equal(prim, prim$1) {
  return prim === prim$1;
}

var compare = Caml_primitive.caml_int_compare;

function hash(prim) {
  return prim;
}

var min = 0;

var max = 1114111;

var bom = 65279;

var rep = 65533;

function unsafe_of_int(prim) {
  return prim;
}

function to_int(prim) {
  return prim;
}

exports.min = min;
exports.max = max;
exports.bom = bom;
exports.rep = rep;
exports.succ = succ;
exports.pred = pred;
exports.is_valid = is_valid;
exports.of_int = of_int;
exports.unsafe_of_int = unsafe_of_int;
exports.to_int = to_int;
exports.is_char = is_char;
exports.of_char = of_char;
exports.to_char = to_char;
exports.unsafe_to_char = unsafe_to_char;
exports.equal = equal;
exports.compare = compare;
exports.hash = hash;
/* No side effect */

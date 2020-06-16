'use strict';

var Caml_format = require("../../lib/js/caml_format.js");

function f(x) {
  if (typeof x === "number") {
    if (x === /* b */98 || x === /* c */99) {
      return 2;
    } else {
      return 1;
    }
  }
  x.HASH === /* e */101;
  if (typeof x === "number" || x.HASH !== /* d */100) {
    return 0;
  } else {
    return x.VAL;
  }
}

function parse_chan(d) {
  if (typeof d === "number") {
    return {
            HASH: /* Error */106380200,
            VAL: "unexpected end of file"
          };
  } else {
    d.HASH === /* Error */106380200;
    return d;
  }
}

function gpr_6359(u) {
  if (typeof u === "number") {
    return 0;
  }
  var variant = u.HASH;
  if (variant === /* A */65) {
    var match = u.VAL;
    return match[0] + match[1] | 0;
  }
  if (variant !== /* D */68) {
    return 0;
  }
  var match$1 = u.VAL;
  return match$1[0] + match$1[1] | 0;
}

function gpr_6359_1(u) {
  if (typeof u === "number") {
    return 0;
  }
  var variant = u.HASH;
  if (variant === /* A */65) {
    var match = u.VAL;
    return Caml_format.caml_int_of_string("1" + match[0]) + match[1] | 0;
  }
  if (variant !== /* D */68) {
    return 0;
  }
  var match$1 = u.VAL;
  return Caml_format.caml_int_of_string("1" + match$1[0]) + match$1[1] | 0;
}

exports.f = f;
exports.parse_chan = parse_chan;
exports.gpr_6359 = gpr_6359;
exports.gpr_6359_1 = gpr_6359_1;
/* No side effect */

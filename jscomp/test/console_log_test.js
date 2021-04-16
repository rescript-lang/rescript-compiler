'use strict';

var Caml_obj = require("../../lib/js/caml_obj.js");

function min_int(prim0, prim1) {
  return Math.min(prim0, prim1);
}

function say(prim0, prim1) {
  return prim0.say(prim1);
}

var v = Caml_obj.caml_compare;

exports.min_int = min_int;
exports.say = say;
exports.v = v;
/* No side effect */

'use strict';

var Caml_option = require("../../lib/js/caml_option.js");

function test1(r) {
  var x = r.f;
  if (x !== undefined) {
    return x.f;
  }
  
}

function getF2(r2) {
  return r2.f2;
}

function test2(r2) {
  var x = r2.f2;
  if (x !== undefined) {
    return Caml_option.valFromOption(x).f2;
  }
  
}

exports.test1 = test1;
exports.getF2 = getF2;
exports.test2 = test2;
/* No side effect */

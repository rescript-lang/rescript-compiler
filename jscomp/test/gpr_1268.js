'use strict';

var Curry = require("../../lib/js/curry");

function f_add2(a, b, x, y) {
  return add(Curry._1(b, y), Curry._1(a, x));
}

exports.f_add2 = f_add2;
/* No side effect */

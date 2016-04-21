// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("../runtime/curry");

function u(_, x) {
  return x + x | 0;
}

function f(g, x) {
  var u = Curry._1(g, x);
  return u + u | 0;
}

exports.u = u;
exports.f = f;
/* No side effect */

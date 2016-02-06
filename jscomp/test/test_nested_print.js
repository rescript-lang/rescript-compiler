// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_curry = require("../runtime/caml_curry");

function u(_, x) {
  return x + x;
}

function f(g, x) {
  var u = Caml_curry.app1(g, x);
  return u + u;
}

exports.u = u;
exports.f = f;
/* No side effect */

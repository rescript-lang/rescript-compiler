// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_curry = require("../runtime/caml_curry");

function f(x) {
  if (x.tag === 248) {
    return Caml_curry.js1(623642069, 1, x);
  }
  else {
    return Caml_curry.app0(x.say_hi);
  }
}

exports.f = f;
/* No side effect */

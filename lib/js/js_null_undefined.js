'use strict';

var Js_primitive = require("./js_primitive.js");

function bind(x, f) {
  if (Js_primitive.is_nil_undef(x)) {
    return x;
  } else {
    return f(x);
  }
}

function iter(x, f) {
  if (Js_primitive.is_nil_undef(x)) {
    return /* () */0;
  } else {
    return f(x);
  }
}

function from_opt(x) {
  if (x) {
    return x[0];
  } else {
    return undefined;
  }
}

exports.bind     = bind;
exports.iter     = iter;
exports.from_opt = from_opt;
/* No side effect */

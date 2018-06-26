'use strict';

var Js_primitive = require("./js_primitive.js");

function bind(x, f) {
  if (x == null) {
    return x;
  } else {
    return f(x);
  }
}

function iter(x, f) {
  if (x == null) {
    return /* () */0;
  } else {
    return f(x);
  }
}

function fromOption(x) {
  if (x !== undefined) {
    return Js_primitive.valFromOption(x);
  } else {
    return undefined;
  }
}

var from_opt = fromOption;

exports.bind = bind;
exports.iter = iter;
exports.fromOption = fromOption;
exports.from_opt = from_opt;
/* No side effect */

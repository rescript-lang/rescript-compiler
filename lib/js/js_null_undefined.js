'use strict';

var Js_primitive = require("./js_primitive");

function bind(x, f) {
  if (Js_primitive.js_is_nil_undef(x)) {
    return undefined;
  }
  else {
    return f(x);
  }
}

exports.bind = bind;
/* No side effect */

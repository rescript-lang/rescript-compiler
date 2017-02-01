'use strict';

import * as Js_primitive from "./js_primitive";

function bind(x, f) {
  if (Js_primitive.js_is_nil_undef(x)) {
    return undefined;
  }
  else {
    return f(x);
  }
}

function iter(x, f) {
  if (Js_primitive.js_is_nil_undef(x)) {
    return /* () */0;
  }
  else {
    return f(x);
  }
}

function from_opt(x) {
  if (x) {
    return x[0];
  }
  else {
    return undefined;
  }
}

export {
  bind     ,
  iter     ,
  from_opt ,
  
}
/* No side effect */

'use strict';


function f(obj) {
  if (typeof obj === "function") {
    return /* () */0;
  } else {
    var size = obj.length;
    var match = size === undefined ? /* None */0 : [size];
    if (match !== /* None */0) {
      console.log(size);
      return /* () */0;
    } else {
      return /* () */0;
    }
  }
}

exports.f = f;
/* No side effect */

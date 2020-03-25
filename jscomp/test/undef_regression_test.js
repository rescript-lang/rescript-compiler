'use strict';


function f(obj) {
  if (typeof obj === "function") {
    return /* () */0;
  }
  var size = obj.length;
  if (size !== undefined) {
    console.log(size);
    return /* () */0;
  } else {
    return /* () */0;
  }
}

exports.f = f;
/* No side effect */

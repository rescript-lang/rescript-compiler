'use strict';


function f(obj) {
  if (typeof obj === "function") {
    return ;
  }
  var size = obj.length;
  if (size !== void 0) {
    console.log(size);
    return ;
  }
  
}

exports.f = f;
/* No side effect */

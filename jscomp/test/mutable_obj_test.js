'use strict';


function f(x) {
  x.dec = (function (x) {
      return {
              x,
              y: x
            };
    });
  
}

exports.f = f;
/* No side effect */

'use strict';


function f(x) {
  x.dec = (function (x) {
      return {
              x: x,
              y: x
            };
    });
  return /* () */0;
}

exports.f = f;
/* No side effect */

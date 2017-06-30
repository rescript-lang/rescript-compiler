'use strict';


function f(x) {
  return x.dec = (function (x) {
            return {
                    x: x,
                    y: x
                  };
          });
}

exports.f = f;
/* No side effect */

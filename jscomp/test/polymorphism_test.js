'use strict';


function map(f, param) {
  if (!param) {
    return /* [] */0;
  }
  var r = f(param.hd);
  return {
          hd: r,
          tl: map(f, param.tl)
        };
}

exports.map = map;
/* No side effect */

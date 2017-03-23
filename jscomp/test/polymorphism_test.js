'use strict';


function map(f, param) {
  if (param) {
    var r = f(param[0]);
    return /* :: */[
            r,
            map(f, param[1])
          ];
  } else {
    return /* [] */0;
  }
}

exports.map = map;
/* No side effect */

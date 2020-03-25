'use strict';


function map(f, param) {
  if (!param) {
    return /* [] */0;
  }
  var r = f(param[0]);
  return /* :: */[
          r,
          map(f, param[1])
        ];
}

exports.map = map;
/* No side effect */

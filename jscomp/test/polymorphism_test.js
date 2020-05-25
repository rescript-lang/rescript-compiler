'use strict';


function map(f, param) {
  if (!param) {
    return /* [] */0;
  }
  var r = f(param._0);
  return /* :: */{
          _0: r,
          _1: map(f, param._1)
        };
}

exports.map = map;
/* No side effect */

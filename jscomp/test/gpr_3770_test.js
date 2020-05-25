'use strict';


function show(param) {
  var a = param._0;
  if (a === 0 && param._1 === 0 && param._2 === 0) {
    return "zeroes";
  }
  return String(a) + String(param._1);
}

exports.show = show;
/* No side effect */

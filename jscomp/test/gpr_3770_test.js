'use strict';


function show(param) {
  var a = param.Arg0;
  if (a === 0 && param.Arg1 === 0 && param.Arg2 === 0) {
    return "zeroes";
  }
  return String(a) + String(param.Arg1);
}

exports.show = show;
/* No side effect */

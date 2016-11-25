'use strict';


function f(x) {
  return x + 3 | 0;
}

function M() {
  var f = function (x) {
    return x;
  };
  return /* module */[/* f */f];
}

exports.f = f;
exports.M = M;
/* No side effect */

'use strict';


function to_unsgined(x) {
  return (x >>> 0);
}

function f(x) {
  return (x >>> 0);
}

function ff(x) {
  return (x >>> 0);
}

function fff(x) {
  return 3 + (3 + (4 + (1 + x)));
}

exports.to_unsgined = to_unsgined;
exports.f = f;
exports.ff = ff;
exports.fff = fff;
/* No side effect */

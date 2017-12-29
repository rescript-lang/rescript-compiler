'use strict';


var v = [0];

function reset() {
  v[0] = 0;
  return /* () */0;
}

function incr() {
  v[0] = v[0] + 1 | 0;
  return /* () */0;
}

exports.v = v;
exports.reset = reset;
exports.incr = incr;
/* No side effect */

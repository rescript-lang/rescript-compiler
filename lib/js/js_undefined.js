'use strict';


function bind(x, f) {
  if (x !== undefined) {
    return f(x);
  }
  else {
    return undefined;
  }
}

function iter(x, f) {
  if (x !== undefined) {
    return f(x);
  }
  else {
    return /* () */0;
  }
}

exports.bind = bind;
exports.iter = iter;
/* No side effect */

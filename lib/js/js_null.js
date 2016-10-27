'use strict';


function bind(x, f) {
  if (x !== null) {
    return f(x);
  }
  else {
    return null;
  }
}

function iter(x, f) {
  if (x !== null) {
    return f(x);
  }
  else {
    return /* () */0;
  }
}

exports.bind = bind;
exports.iter = iter;
/* No side effect */

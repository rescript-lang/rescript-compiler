'use strict';


function bind(x, f) {
  if (x !== null) {
    return f(x);
  } else {
    return null;
  }
}

function iter(x, f) {
  if (x !== null) {
    return f(x);
  } else {
    return /* () */0;
  }
}

function from_opt(x) {
  if (x) {
    return x[0];
  } else {
    return null;
  }
}

exports.bind = bind;
exports.iter = iter;
exports.from_opt = from_opt;
/* No side effect */

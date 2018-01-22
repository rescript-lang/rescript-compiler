'use strict';


function map(f, x) {
  if (x == null) {
    return x;
  } else {
    return f(x);
  }
}

function andThen(f, x) {
  if (x == null) {
    return x;
  } else {
    return f(x);
  }
}

function bind(x, f) {
  return map(f, x);
}

function iter(x, f) {
  if (x == null) {
    return /* () */0;
  } else {
    return f(x);
  }
}

function from_opt(x) {
  if (x) {
    return x[0];
  } else {
    return undefined;
  }
}

exports.map = map;
exports.andThen = andThen;
exports.bind = bind;
exports.iter = iter;
exports.from_opt = from_opt;
/* No side effect */

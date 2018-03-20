'use strict';


function test(x) {
  return x === null;
}

function getExn(f) {
  if (f !== null) {
    return f;
  } else {
    throw new Error("Js.Null.getExn");
  }
}

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

function fromOption(x) {
  if (x) {
    return x[0];
  } else {
    return null;
  }
}

var from_opt = fromOption;

exports.test = test;
exports.getExn = getExn;
exports.bind = bind;
exports.iter = iter;
exports.fromOption = fromOption;
exports.from_opt = from_opt;
/* No side effect */

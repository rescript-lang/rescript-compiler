'use strict';


function test(x) {
  return x === null;
}

function getExn(f) {
  var match = f === null ? /* None */0 : [f];
  if (match !== /* None */0) {
    return f;
  } else {
    throw new Error("Js.Null.getExn");
  }
}

function bind(x, f) {
  var match = x === null ? /* None */0 : [x];
  if (match !== /* None */0) {
    return f(x);
  } else {
    return null;
  }
}

function iter(x, f) {
  var match = x === null ? /* None */0 : [x];
  if (match !== /* None */0) {
    return f(x);
  } else {
    return /* () */0;
  }
}

function fromOption(x) {
  if (x !== /* None */0) {
    return x[/* None */0];
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

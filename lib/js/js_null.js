'use strict';


function castExn(f) {
  if (f !== null) {
    return f;
  } else {
    throw new Error("Js.Null.castExn");
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

exports.castExn = castExn;
exports.bind = bind;
exports.iter = iter;
exports.fromOption = fromOption;
exports.from_opt = from_opt;
/* No side effect */

'use strict';


function bind(x, f) {
  var match = (x == null) ? /* None */0 : [x];
  if (match !== /* None */0) {
    return f(x);
  } else {
    return x;
  }
}

function iter(x, f) {
  var match = (x == null) ? /* None */0 : [x];
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
    return undefined;
  }
}

var from_opt = fromOption;

exports.bind = bind;
exports.iter = iter;
exports.fromOption = fromOption;
exports.from_opt = from_opt;
/* No side effect */

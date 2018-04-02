'use strict';


function some(x) {
  return /* Some */[x];
}

function is_none(x) {
  return x === /* None */0;
}

function to_def(x) {
  if (x) {
    return x[0];
  } else {
    return undefined;
  }
}

function cons(x, y) {
  return /* :: */[
          x,
          y
        ];
}

function is_list_empty(x) {
  return x === /* [] */0;
}

var none = /* None */0;

exports.none = none;
exports.some = some;
exports.is_none = is_none;
exports.to_def = to_def;
exports.cons = cons;
exports.is_list_empty = is_list_empty;
/* No side effect */

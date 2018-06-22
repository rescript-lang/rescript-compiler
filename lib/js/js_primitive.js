'use strict';


function nullable_to_opt(x) {
  if (x === null || x === undefined) {
    return /* None */0;
  } else {
    return /* Some */[x];
  }
}

function undefined_to_opt(x) {
  if (x === undefined) {
    return /* None */0;
  } else {
    return /* Some */[x];
  }
}

function null_to_opt(x) {
  if (x === null) {
    return /* None */0;
  } else {
    return /* Some */[x];
  }
}

function option_get(x) {
  if (x !== /* None */0) {
    return x[0];
  } else {
    return undefined;
  }
}

function option_get_unwrap(x) {
  if (x !== /* None */0) {
    return x[0][1];
  } else {
    return undefined;
  }
}

exports.nullable_to_opt = nullable_to_opt;
exports.undefined_to_opt = undefined_to_opt;
exports.null_to_opt = null_to_opt;
exports.option_get = option_get;
exports.option_get_unwrap = option_get_unwrap;
/* No side effect */

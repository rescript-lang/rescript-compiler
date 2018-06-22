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

var undefinedHeader = /* array */[];

function valFromOption(x) {
  if (x !== null && x[0] === undefinedHeader) {
    var depth = x[1];
    if (depth === 0) {
      return /* None */0;
    } else {
      return /* tuple */[
              undefinedHeader,
              depth - 1 | 0
            ];
    }
  } else {
    return x;
  }
}

function some(x) {
  if (x === /* None */0) {
    return /* tuple */[
            undefinedHeader,
            0
          ];
  } else if (x !== null && x[0] === undefinedHeader) {
    return /* tuple */[
            undefinedHeader,
            x[1] + 1 | 0
          ];
  } else {
    return x;
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
exports.valFromOption = valFromOption;
exports.some = some;
exports.option_get = option_get;
exports.option_get_unwrap = option_get_unwrap;
/* No side effect */

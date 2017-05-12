'use strict';


function some(x) {
  return /* Some */[x];
}

function isSome(param) {
  if (param) {
    return /* true */1;
  } else {
    return /* false */0;
  }
}

function isNone(param) {
  if (param) {
    return /* false */0;
  } else {
    return /* true */1;
  }
}

function getExn(x) {
  if (x) {
    return x[0];
  } else {
    var str = " " + (String("File \"bs_option.ml\", line 39, characters 34-42") + ": Bs.Option.unsafeGet");
    throw new Error(str);
  }
}

function equal(eq, a, b) {
  if (a) {
    if (b) {
      return eq(a[0], b[0]);
    } else {
      return /* false */0;
    }
  } else {
    return +(b === /* None */0);
  }
}

function andThen(f, x) {
  if (x) {
    return f(x[0]);
  } else {
    return /* None */0;
  }
}

exports.some    = some;
exports.isSome  = isSome;
exports.isNone  = isNone;
exports.getExn  = getExn;
exports.equal   = equal;
exports.andThen = andThen;
/* No side effect */

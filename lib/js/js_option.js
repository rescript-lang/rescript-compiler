'use strict';


function some(x) {
  return /* Some */[x];
}

function isSome(param) {
  if (param) {
    return true;
  } else {
    return false;
  }
}

function isSomeValue(eq, v, x) {
  if (x) {
    return eq(v, x[0]);
  } else {
    return false;
  }
}

function isNone(param) {
  if (param) {
    return false;
  } else {
    return true;
  }
}

function getExn(x) {
  if (x) {
    return x[0];
  } else {
    throw new Error("getExn");
  }
}

function equal(eq, a, b) {
  if (a) {
    if (b) {
      return eq(a[0], b[0]);
    } else {
      return false;
    }
  } else {
    return b === /* None */0;
  }
}

function andThen(f, x) {
  if (x) {
    return f(x[0]);
  } else {
    return /* None */0;
  }
}

function map(f, x) {
  if (x) {
    return /* Some */[f(x[0])];
  } else {
    return /* None */0;
  }
}

function getWithDefault(a, x) {
  if (x) {
    return x[0];
  } else {
    return a;
  }
}

function filter(f, x) {
  if (x) {
    var x$1 = x[0];
    if (f(x$1)) {
      return /* Some */[x$1];
    } else {
      return /* None */0;
    }
  } else {
    return /* None */0;
  }
}

function firstSome(a, b) {
  if (a) {
    return a;
  } else if (b) {
    return b;
  } else {
    return /* None */0;
  }
}

var $$default = getWithDefault;

exports.some = some;
exports.isSome = isSome;
exports.isSomeValue = isSomeValue;
exports.isNone = isNone;
exports.getExn = getExn;
exports.equal = equal;
exports.andThen = andThen;
exports.map = map;
exports.getWithDefault = getWithDefault;
exports.$$default = $$default;
exports.default = $$default;
exports.__esModule = true;
exports.filter = filter;
exports.firstSome = firstSome;
/* No side effect */

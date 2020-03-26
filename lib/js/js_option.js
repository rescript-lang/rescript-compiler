'use strict';

var Caml_option = require("./caml_option.js");

function some(x) {
  return Caml_option.some(x);
}

function isSome(param) {
  return param !== void 0;
}

function isSomeValue(eq, v, x) {
  if (x !== void 0) {
    return eq(v, Caml_option.valFromOption(x));
  } else {
    return false;
  }
}

function isNone(param) {
  return param === void 0;
}

function getExn(x) {
  if (x !== void 0) {
    return Caml_option.valFromOption(x);
  }
  throw new Error("getExn");
}

function equal(eq, a, b) {
  if (a !== void 0) {
    if (b !== void 0) {
      return eq(Caml_option.valFromOption(a), Caml_option.valFromOption(b));
    } else {
      return false;
    }
  } else {
    return b === void 0;
  }
}

function andThen(f, x) {
  if (x !== void 0) {
    return f(Caml_option.valFromOption(x));
  }
  
}

function map(f, x) {
  if (x !== void 0) {
    return Caml_option.some(f(Caml_option.valFromOption(x)));
  }
  
}

function getWithDefault(a, x) {
  if (x !== void 0) {
    return Caml_option.valFromOption(x);
  } else {
    return a;
  }
}

function filter(f, x) {
  if (x === void 0) {
    return ;
  }
  var x$1 = Caml_option.valFromOption(x);
  if (f(x$1)) {
    return Caml_option.some(x$1);
  }
  
}

function firstSome(a, b) {
  if (a !== void 0) {
    return a;
  } else if (b !== void 0) {
    return b;
  } else {
    return ;
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

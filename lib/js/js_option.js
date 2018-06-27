'use strict';

var Js_primitive = require("./js_primitive.js");

function some(x) {
  return Js_primitive.some(x);
}

function isSome(param) {
  return param !== undefined;
}

function isSomeValue(eq, v, x) {
  if (x !== undefined) {
    return eq(v, Js_primitive.valFromOption(x));
  } else {
    return false;
  }
}

function isNone(param) {
  return param === undefined;
}

function getExn(x) {
  if (x !== undefined) {
    return Js_primitive.valFromOption(x);
  } else {
    throw new Error("getExn");
  }
}

function equal(eq, a, b) {
  if (a !== undefined) {
    if (b !== undefined) {
      return eq(Js_primitive.valFromOption(a), Js_primitive.valFromOption(b));
    } else {
      return false;
    }
  } else {
    return b === undefined;
  }
}

function andThen(f, x) {
  if (x !== undefined) {
    return f(Js_primitive.valFromOption(x));
  }
  
}

function map(f, x) {
  if (x !== undefined) {
    return Js_primitive.some(f(Js_primitive.valFromOption(x)));
  }
  
}

function getWithDefault(a, x) {
  if (x !== undefined) {
    return Js_primitive.valFromOption(x);
  } else {
    return a;
  }
}

function filter(f, x) {
  if (x !== undefined) {
    var x$1 = Js_primitive.valFromOption(x);
    if (f(x$1)) {
      return Js_primitive.some(x$1);
    } else {
      return undefined;
    }
  }
  
}

function firstSome(a, b) {
  if (a !== undefined) {
    return a;
  } else if (b !== undefined) {
    return b;
  } else {
    return undefined;
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

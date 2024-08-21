'use strict';

let Js_exn = require("./js_exn.js");
let Caml_option = require("./caml_option.js");

function some(x) {
  return Caml_option.some(x);
}

function isSome(x) {
  return x !== undefined;
}

function isSomeValue(eq, v, x) {
  if (x !== undefined) {
    return eq(v, Caml_option.valFromOption(x));
  } else {
    return false;
  }
}

function isNone(x) {
  return x === undefined;
}

function getExn(x) {
  if (x !== undefined) {
    return Caml_option.valFromOption(x);
  } else {
    return Js_exn.raiseError("getExn");
  }
}

function equal(eq, a, b) {
  if (a !== undefined) {
    if (b !== undefined) {
      return eq(Caml_option.valFromOption(a), Caml_option.valFromOption(b));
    } else {
      return false;
    }
  } else {
    return b === undefined;
  }
}

function andThen(f, x) {
  if (x !== undefined) {
    return f(Caml_option.valFromOption(x));
  }
  
}

function map(f, x) {
  if (x !== undefined) {
    return Caml_option.some(f(Caml_option.valFromOption(x)));
  }
  
}

function getWithDefault(a, x) {
  if (x !== undefined) {
    return Caml_option.valFromOption(x);
  } else {
    return a;
  }
}

function filter(f, x) {
  if (x === undefined) {
    return;
  }
  let x$1 = Caml_option.valFromOption(x);
  if (f(x$1)) {
    return Caml_option.some(x$1);
  }
  
}

function firstSome(a, b) {
  if (a !== undefined) {
    return a;
  } else if (b !== undefined) {
    return b;
  } else {
    return;
  }
}

let $$default = getWithDefault;

exports.some = some;
exports.isSome = isSome;
exports.isSomeValue = isSomeValue;
exports.isNone = isNone;
exports.getExn = getExn;
exports.equal = equal;
exports.andThen = andThen;
exports.map = map;
exports.getWithDefault = getWithDefault;
exports.default = $$default;
exports.__esModule = true;
exports.filter = filter;
exports.firstSome = firstSome;
/* No side effect */

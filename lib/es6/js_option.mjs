

import * as Caml_option from "./caml_option.mjs";

function some(x) {
  return Caml_option.some(x);
}

function isSome(param) {
  return param !== undefined;
}

function isSomeValue(eq, v, x) {
  if (x !== undefined) {
    return eq(v, Caml_option.valFromOption(x));
  } else {
    return false;
  }
}

function isNone(param) {
  return param === undefined;
}

function getExn(x) {
  if (x !== undefined) {
    return Caml_option.valFromOption(x);
  }
  throw new Error("getExn");
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
    return ;
  }
  var x$1 = Caml_option.valFromOption(x);
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
    return ;
  }
}

var $$default = getWithDefault;

export {
  some ,
  isSome ,
  isSomeValue ,
  isNone ,
  getExn ,
  equal ,
  andThen ,
  map ,
  getWithDefault ,
  $$default ,
  $$default as default,
  filter ,
  firstSome ,
  
}
/* No side effect */

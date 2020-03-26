

import * as Curry from "./curry.js";
import * as Caml_option from "./caml_option.js";

function forEachU(opt, f) {
  if (opt !== void 0) {
    return f(Caml_option.valFromOption(opt));
  }
  
}

function forEach(opt, f) {
  return forEachU(opt, Curry.__1(f));
}

function getExn(param) {
  if (param !== void 0) {
    return Caml_option.valFromOption(param);
  }
  throw new Error("getExn");
}

function mapWithDefaultU(opt, $$default, f) {
  if (opt !== void 0) {
    return f(Caml_option.valFromOption(opt));
  } else {
    return $$default;
  }
}

function mapWithDefault(opt, $$default, f) {
  return mapWithDefaultU(opt, $$default, Curry.__1(f));
}

function mapU(opt, f) {
  if (opt !== void 0) {
    return Caml_option.some(f(Caml_option.valFromOption(opt)));
  }
  
}

function map(opt, f) {
  return mapU(opt, Curry.__1(f));
}

function flatMapU(opt, f) {
  if (opt !== void 0) {
    return f(Caml_option.valFromOption(opt));
  }
  
}

function flatMap(opt, f) {
  return flatMapU(opt, Curry.__1(f));
}

function getWithDefault(opt, $$default) {
  if (opt !== void 0) {
    return Caml_option.valFromOption(opt);
  } else {
    return $$default;
  }
}

function isSome(param) {
  return param !== void 0;
}

function isNone(x) {
  return x === void 0;
}

function eqU(a, b, f) {
  if (a !== void 0) {
    if (b !== void 0) {
      return f(Caml_option.valFromOption(a), Caml_option.valFromOption(b));
    } else {
      return false;
    }
  } else {
    return b === void 0;
  }
}

function eq(a, b, f) {
  return eqU(a, b, Curry.__2(f));
}

function cmpU(a, b, f) {
  if (a !== void 0) {
    if (b !== void 0) {
      return f(Caml_option.valFromOption(a), Caml_option.valFromOption(b));
    } else {
      return 1;
    }
  } else if (b !== void 0) {
    return -1;
  } else {
    return 0;
  }
}

function cmp(a, b, f) {
  return cmpU(a, b, Curry.__2(f));
}

export {
  forEachU ,
  forEach ,
  getExn ,
  mapWithDefaultU ,
  mapWithDefault ,
  mapU ,
  map ,
  flatMapU ,
  flatMap ,
  getWithDefault ,
  isSome ,
  isNone ,
  eqU ,
  eq ,
  cmpU ,
  cmp ,
  
}
/* No side effect */

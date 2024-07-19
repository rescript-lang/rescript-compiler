

import * as Curry from "./curry.js";
import * as Caml_option from "./caml_option.js";

function keepU(opt, p) {
  if (opt !== undefined && Curry._1(p, Caml_option.valFromOption(opt))) {
    return opt;
  }
  
}

function keep(opt, p) {
  return keepU(opt, Curry.__1(p));
}

function forEachU(opt, f) {
  if (opt !== undefined) {
    return Curry._1(f, Caml_option.valFromOption(opt));
  }
  
}

function forEach(opt, f) {
  forEachU(opt, Curry.__1(f));
}

function getExn(x) {
  if (x !== undefined) {
    return Caml_option.valFromOption(x);
  }
  throw new Error("Not_found", {
        cause: {
          RE_EXN_ID: "Not_found"
        }
      });
}

function mapWithDefaultU(opt, $$default, f) {
  if (opt !== undefined) {
    return Curry._1(f, Caml_option.valFromOption(opt));
  } else {
    return $$default;
  }
}

function mapWithDefault(opt, $$default, f) {
  return mapWithDefaultU(opt, $$default, Curry.__1(f));
}

function mapU(opt, f) {
  if (opt !== undefined) {
    return Caml_option.some(Curry._1(f, Caml_option.valFromOption(opt)));
  }
  
}

function map(opt, f) {
  return mapU(opt, Curry.__1(f));
}

function flatMapU(opt, f) {
  if (opt !== undefined) {
    return Curry._1(f, Caml_option.valFromOption(opt));
  }
  
}

function flatMap(opt, f) {
  return flatMapU(opt, Curry.__1(f));
}

function getWithDefault(opt, $$default) {
  if (opt !== undefined) {
    return Caml_option.valFromOption(opt);
  } else {
    return $$default;
  }
}

function orElse(opt, other) {
  if (opt !== undefined) {
    return opt;
  } else {
    return other;
  }
}

function isSome(x) {
  return x !== undefined;
}

function isNone(x) {
  return x === undefined;
}

function eqU(a, b, f) {
  if (a !== undefined) {
    if (b !== undefined) {
      return Curry._2(f, Caml_option.valFromOption(a), Caml_option.valFromOption(b));
    } else {
      return false;
    }
  } else {
    return b === undefined;
  }
}

function eq(a, b, f) {
  return eqU(a, b, Curry.__2(f));
}

function cmpU(a, b, f) {
  if (a !== undefined) {
    if (b !== undefined) {
      return Curry._2(f, Caml_option.valFromOption(a), Caml_option.valFromOption(b));
    } else {
      return 1;
    }
  } else if (b !== undefined) {
    return -1;
  } else {
    return 0;
  }
}

function cmp(a, b, f) {
  return cmpU(a, b, Curry.__2(f));
}

export {
  keepU,
  keep,
  forEachU,
  forEach,
  getExn,
  mapWithDefaultU,
  mapWithDefault,
  mapU,
  map,
  flatMapU,
  flatMap,
  getWithDefault,
  orElse,
  isSome,
  isNone,
  eqU,
  eq,
  cmpU,
  cmp,
}
/* No side effect */

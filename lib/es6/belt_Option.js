

import * as Caml_option from "./caml_option.js";

function keep(opt, p) {
  if (opt !== undefined && p(Caml_option.valFromOption(opt))) {
    return opt;
  }
  
}

function forEach(opt, f) {
  if (opt !== undefined) {
    return f(Caml_option.valFromOption(opt));
  }
  
}

function getExn(x) {
  if (x !== undefined) {
    return Caml_option.valFromOption(x);
  }
  throw {
    RE_EXN_ID: "Not_found",
    Error: new Error()
  };
}

function mapWithDefault(opt, $$default, f) {
  if (opt !== undefined) {
    return f(Caml_option.valFromOption(opt));
  } else {
    return $$default;
  }
}

function map(opt, f) {
  if (opt !== undefined) {
    return Caml_option.some(f(Caml_option.valFromOption(opt)));
  }
  
}

function flatMap(opt, f) {
  if (opt !== undefined) {
    return f(Caml_option.valFromOption(opt));
  }
  
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

function eq(a, b, f) {
  if (a !== undefined) {
    if (b !== undefined) {
      return f(Caml_option.valFromOption(a), Caml_option.valFromOption(b));
    } else {
      return false;
    }
  } else {
    return b === undefined;
  }
}

function cmp(a, b, f) {
  if (a !== undefined) {
    if (b !== undefined) {
      return f(Caml_option.valFromOption(a), Caml_option.valFromOption(b));
    } else {
      return 1;
    }
  } else if (b !== undefined) {
    return -1;
  } else {
    return 0;
  }
}

let keepU = keep;

let forEachU = forEach;

let mapWithDefaultU = mapWithDefault;

let mapU = map;

let flatMapU = flatMap;

let eqU = eq;

let cmpU = cmp;

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

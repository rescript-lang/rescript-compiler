

import * as $$Error from "./Error.js";
import * as Primitive_option from "./Primitive_option.js";

function filter(opt, p) {
  if (opt !== undefined && p(Primitive_option.valFromOption(opt))) {
    return opt;
  }
  
}

function forEach(opt, f) {
  if (opt !== undefined) {
    return f(Primitive_option.valFromOption(opt));
  }
  
}

function getExn(x, message) {
  if (x !== undefined) {
    return Primitive_option.valFromOption(x);
  } else {
    return $$Error.panic(message !== undefined ? message : "Option.getExn called for None value");
  }
}

function mapOr(opt, $$default, f) {
  if (opt !== undefined) {
    return f(Primitive_option.valFromOption(opt));
  } else {
    return $$default;
  }
}

function map(opt, f) {
  if (opt !== undefined) {
    return Primitive_option.some(f(Primitive_option.valFromOption(opt)));
  }
  
}

function flatMap(opt, f) {
  if (opt !== undefined) {
    return f(Primitive_option.valFromOption(opt));
  }
  
}

function getOr(opt, $$default) {
  if (opt !== undefined) {
    return Primitive_option.valFromOption(opt);
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

function equal(a, b, eq) {
  if (a !== undefined) {
    if (b !== undefined) {
      return eq(Primitive_option.valFromOption(a), Primitive_option.valFromOption(b));
    } else {
      return false;
    }
  } else {
    return b === undefined;
  }
}

function compare(a, b, cmp) {
  if (a !== undefined) {
    if (b !== undefined) {
      return cmp(Primitive_option.valFromOption(a), Primitive_option.valFromOption(b));
    } else {
      return 1;
    }
  } else if (b !== undefined) {
    return -1;
  } else {
    return 0;
  }
}

let mapWithDefault = mapOr;

let getWithDefault = getOr;

export {
  filter,
  forEach,
  getExn,
  mapOr,
  mapWithDefault,
  map,
  flatMap,
  getOr,
  getWithDefault,
  orElse,
  isSome,
  isNone,
  equal,
  compare,
}
/* No side effect */

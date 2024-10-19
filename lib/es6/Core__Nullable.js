

import * as Core__Option from "./Core__Option.js";
import * as Primitive_option from "./Primitive_option.js";

function fromOption(option) {
  if (option !== undefined) {
    return Primitive_option.valFromOption(option);
  }
  
}

function equal(a, b, eq) {
  return Core__Option.equal(a === null ? undefined : Primitive_option.some(a), b === null ? undefined : Primitive_option.some(b), eq);
}

function compare(a, b, cmp) {
  return Core__Option.compare(a === null ? undefined : Primitive_option.some(a), b === null ? undefined : Primitive_option.some(b), cmp);
}

function getOr(value, $$default) {
  if (value !== null) {
    return value;
  } else {
    return $$default;
  }
}

function getExn(value) {
  if (value !== null) {
    return value;
  }
  throw {
    RE_EXN_ID: "Invalid_argument",
    _1: "Nullable.getExn: value is null or undefined",
    Error: new Error()
  };
}

function forEach(value, f) {
  if (value !== null) {
    return f(value);
  }
  
}

function map(value, f) {
  if (value !== null) {
    return f(value);
  } else {
    return value;
  }
}

function mapOr(value, $$default, f) {
  if (value !== null) {
    return f(value);
  } else {
    return $$default;
  }
}

function flatMap(value, f) {
  if (value !== null) {
    return f(value);
  } else {
    return value;
  }
}

let getWithDefault = getOr;

let mapWithDefault = mapOr;

export {
  equal,
  compare,
  fromOption,
  getOr,
  getWithDefault,
  getExn,
  forEach,
  map,
  mapOr,
  mapWithDefault,
  flatMap,
}
/* No side effect */

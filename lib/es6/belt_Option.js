

import * as Caml_option from "./caml_option.js";

function keepU(opt, p) {
  if (opt !== undefined && p(Caml_option.valFromOption(opt))) {
    return opt;
  }
  
}

function keep(opt, p) {
  return keepU(opt, (function (x) {
    return p(x);
  }));
}

function forEachU(opt, f) {
  if (opt !== undefined) {
    return f(Caml_option.valFromOption(opt));
  }
  
}

function forEach(opt, f) {
  forEachU(opt, (function (x) {
    f(x);
  }));
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
    return f(Caml_option.valFromOption(opt));
  } else {
    return $$default;
  }
}

function mapWithDefault(opt, $$default, f) {
  return mapWithDefaultU(opt, $$default, (function (x) {
    return f(x);
  }));
}

function mapU(opt, f) {
  if (opt !== undefined) {
    return Caml_option.some(f(Caml_option.valFromOption(opt)));
  }
  
}

function map(opt, f) {
  return mapU(opt, (function (x) {
    return f(x);
  }));
}

function flatMapU(opt, f) {
  if (opt !== undefined) {
    return f(Caml_option.valFromOption(opt));
  }
  
}

function flatMap(opt, f) {
  return flatMapU(opt, (function (x) {
    return f(x);
  }));
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
      return f(Caml_option.valFromOption(a), Caml_option.valFromOption(b));
    } else {
      return false;
    }
  } else {
    return b === undefined;
  }
}

function eq(a, b, f) {
  return eqU(a, b, (function (x, y) {
    return f(x, y);
  }));
}

function cmpU(a, b, f) {
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

function cmp(a, b, f) {
  return cmpU(a, b, (function (x, y) {
    return f(x, y);
  }));
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



import * as Curry from "./curry.js";

function getExn(x) {
  if (x.TAG === /* Ok */0) {
    return x._0;
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

function mapWithDefaultU(opt, $$default, f) {
  if (opt.TAG === /* Ok */0) {
    return f(opt._0);
  } else {
    return $$default;
  }
}

function mapWithDefault(opt, $$default, f) {
  return mapWithDefaultU(opt, $$default, Curry.__1(f));
}

function mapU(opt, f) {
  if (opt.TAG === /* Ok */0) {
    return {
            TAG: /* Ok */0,
            _0: f(opt._0)
          };
  } else {
    return {
            TAG: /* Error */1,
            _0: opt._0
          };
  }
}

function map(opt, f) {
  return mapU(opt, Curry.__1(f));
}

function flatMapU(opt, f) {
  if (opt.TAG === /* Ok */0) {
    return f(opt._0);
  } else {
    return {
            TAG: /* Error */1,
            _0: opt._0
          };
  }
}

function flatMap(opt, f) {
  return flatMapU(opt, Curry.__1(f));
}

function getWithDefault(opt, $$default) {
  if (opt.TAG === /* Ok */0) {
    return opt._0;
  } else {
    return $$default;
  }
}

function isOk(param) {
  if (param.TAG === /* Ok */0) {
    return true;
  } else {
    return false;
  }
}

function isError(param) {
  if (param.TAG === /* Ok */0) {
    return false;
  } else {
    return true;
  }
}

function eqU(a, b, f) {
  if (a.TAG === /* Ok */0) {
    if (b.TAG === /* Ok */0) {
      return f(a._0, b._0);
    } else {
      return false;
    }
  } else if (b.TAG === /* Ok */0) {
    return false;
  } else {
    return true;
  }
}

function eq(a, b, f) {
  return eqU(a, b, Curry.__2(f));
}

function cmpU(a, b, f) {
  if (a.TAG === /* Ok */0) {
    if (b.TAG === /* Ok */0) {
      return f(a._0, b._0);
    } else {
      return 1;
    }
  } else if (b.TAG === /* Ok */0) {
    return -1;
  } else {
    return 0;
  }
}

function cmp(a, b, f) {
  return cmpU(a, b, Curry.__2(f));
}

export {
  getExn ,
  mapWithDefaultU ,
  mapWithDefault ,
  mapU ,
  map ,
  flatMapU ,
  flatMap ,
  getWithDefault ,
  isOk ,
  isError ,
  eqU ,
  eq ,
  cmpU ,
  cmp ,
  
}
/* No side effect */

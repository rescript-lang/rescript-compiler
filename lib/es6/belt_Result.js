

import * as Block from "./block.js";
import * as Curry from "./curry.js";

function getExn(param) {
  if (param.tag) {
    throw new Error("getExn");
  } else {
    return param[0];
  }
}

function mapWithDefaultU(opt, $$default, f) {
  if (opt.tag) {
    return $$default;
  } else {
    return f(opt[0]);
  }
}

function mapWithDefault(opt, $$default, f) {
  return mapWithDefaultU(opt, $$default, Curry.__1(f));
}

function mapU(opt, f) {
  if (opt.tag) {
    return /* Error */Block.__(1, [opt[0]]);
  } else {
    return /* Ok */Block.__(0, [f(opt[0])]);
  }
}

function map(opt, f) {
  return mapU(opt, Curry.__1(f));
}

function flatMapU(opt, f) {
  if (opt.tag) {
    return /* Error */Block.__(1, [opt[0]]);
  } else {
    return f(opt[0]);
  }
}

function flatMap(opt, f) {
  return flatMapU(opt, Curry.__1(f));
}

function getWithDefault(opt, $$default) {
  if (opt.tag) {
    return $$default;
  } else {
    return opt[0];
  }
}

function isOk(param) {
  if (param.tag) {
    return false;
  } else {
    return true;
  }
}

function isError(param) {
  if (param.tag) {
    return true;
  } else {
    return false;
  }
}

function eqU(a, b, f) {
  if (a.tag) {
    if (b.tag) {
      return true;
    } else {
      return false;
    }
  } else if (b.tag) {
    return false;
  } else {
    return f(a[0], b[0]);
  }
}

function eq(a, b, f) {
  return eqU(a, b, Curry.__2(f));
}

function cmpU(a, b, f) {
  if (a.tag) {
    if (b.tag) {
      return 0;
    } else {
      return -1;
    }
  } else if (b.tag) {
    return 1;
  } else {
    return f(a[0], b[0]);
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

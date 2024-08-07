


function getExn(x) {
  if (x.TAG === "Ok") {
    return x._0;
  }
  throw new Error("Not_found", {
    cause: {
      RE_EXN_ID: "Not_found"
    }
  });
}

function mapWithDefault(opt, $$default, f) {
  if (opt.TAG === "Ok") {
    return f(opt._0);
  } else {
    return $$default;
  }
}

function map(opt, f) {
  if (opt.TAG === "Ok") {
    return {
      TAG: "Ok",
      _0: f(opt._0)
    };
  } else {
    return {
      TAG: "Error",
      _0: opt._0
    };
  }
}

function flatMap(opt, f) {
  if (opt.TAG === "Ok") {
    return f(opt._0);
  } else {
    return {
      TAG: "Error",
      _0: opt._0
    };
  }
}

function getWithDefault(opt, $$default) {
  if (opt.TAG === "Ok") {
    return opt._0;
  } else {
    return $$default;
  }
}

function isOk(x) {
  if (x.TAG === "Ok") {
    return true;
  } else {
    return false;
  }
}

function isError(x) {
  if (x.TAG === "Ok") {
    return false;
  } else {
    return true;
  }
}

function eq(a, b, f) {
  if (a.TAG === "Ok") {
    if (b.TAG === "Ok") {
      return f(a._0, b._0);
    } else {
      return false;
    }
  } else if (b.TAG === "Ok") {
    return false;
  } else {
    return true;
  }
}

function cmp(a, b, f) {
  if (a.TAG === "Ok") {
    if (b.TAG === "Ok") {
      return f(a._0, b._0);
    } else {
      return 1;
    }
  } else if (b.TAG === "Ok") {
    return -1;
  } else {
    return 0;
  }
}

let mapWithDefaultU = mapWithDefault;

let mapU = map;

let flatMapU = flatMap;

let eqU = eq;

let cmpU = cmp;

export {
  getExn,
  mapWithDefaultU,
  mapWithDefault,
  mapU,
  map,
  flatMapU,
  flatMap,
  getWithDefault,
  isOk,
  isError,
  eqU,
  eq,
  cmpU,
  cmp,
}
/* No side effect */

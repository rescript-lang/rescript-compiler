


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

function mapWithDefaultU(opt, $$default, f) {
  if (opt.TAG === "Ok") {
    return f(opt._0);
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

function map(opt, f) {
  return mapU(opt, (function (x) {
    return f(x);
  }));
}

function flatMapU(opt, f) {
  if (opt.TAG === "Ok") {
    return f(opt._0);
  } else {
    return {
      TAG: "Error",
      _0: opt._0
    };
  }
}

function flatMap(opt, f) {
  return flatMapU(opt, (function (x) {
    return f(x);
  }));
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

function eqU(a, b, f) {
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

function eq(a, b, f) {
  return eqU(a, b, (function (x, y) {
    return f(x, y);
  }));
}

function cmpU(a, b, f) {
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

function cmp(a, b, f) {
  return cmpU(a, b, (function (x, y) {
    return f(x, y);
  }));
}

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

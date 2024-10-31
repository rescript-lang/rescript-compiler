'use strict';


function getExn(x) {
  if (x.TAG === "Ok") {
    return x._0;
  }
  throw {
    RE_EXN_ID: "Not_found",
    Error: new Error()
  };
}

function mapOr(opt, $$default, f) {
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
    return opt;
  }
}

function flatMap(opt, f) {
  if (opt.TAG === "Ok") {
    return f(opt._0);
  } else {
    return opt;
  }
}

function getOr(opt, $$default) {
  if (opt.TAG === "Ok") {
    return opt._0;
  } else {
    return $$default;
  }
}

function isOk(x) {
  return x.TAG === "Ok";
}

function isError(x) {
  if (x.TAG === "Ok") {
    return false;
  } else {
    return true;
  }
}

function equal(a, b, f) {
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

function compare(a, b, f) {
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

function forEach(r, f) {
  if (r.TAG === "Ok") {
    return f(r._0);
  }
  
}

function mapError(r, f) {
  if (r.TAG === "Ok") {
    return r;
  } else {
    return {
      TAG: "Error",
      _0: f(r._0)
    };
  }
}

let mapWithDefault = mapOr;

let getWithDefault = getOr;

exports.getExn = getExn;
exports.mapOr = mapOr;
exports.mapWithDefault = mapWithDefault;
exports.map = map;
exports.flatMap = flatMap;
exports.getOr = getOr;
exports.getWithDefault = getWithDefault;
exports.isOk = isOk;
exports.isError = isError;
exports.equal = equal;
exports.compare = compare;
exports.forEach = forEach;
exports.mapError = mapError;
/* No side effect */

'use strict';

var Curry = require("./curry.js");

function getExn(param) {
  if (/* XXX */param.tag === "Ok") {
    return param.Arg0;
  } else {
    throw new Error("getExn");
  }
}

function mapWithDefaultU(opt, $$default, f) {
  if (/* XXX */opt.tag === "Ok") {
    return f(opt.Arg0);
  } else {
    return $$default;
  }
}

function mapWithDefault(opt, $$default, f) {
  return mapWithDefaultU(opt, $$default, Curry.__1(f));
}

function mapU(opt, f) {
  if (/* XXX */opt.tag === "Ok") {
    return /* constructor */{
            tag: "Ok",
            Arg0: f(opt.Arg0)
          };
  } else {
    return /* constructor */{
            tag: "Error",
            Arg0: opt.Arg0
          };
  }
}

function map(opt, f) {
  return mapU(opt, Curry.__1(f));
}

function flatMapU(opt, f) {
  if (/* XXX */opt.tag === "Ok") {
    return f(opt.Arg0);
  } else {
    return /* constructor */{
            tag: "Error",
            Arg0: opt.Arg0
          };
  }
}

function flatMap(opt, f) {
  return flatMapU(opt, Curry.__1(f));
}

function getWithDefault(opt, $$default) {
  if (/* XXX */opt.tag === "Ok") {
    return opt.Arg0;
  } else {
    return $$default;
  }
}

function isOk(param) {
  if (/* XXX */param.tag === "Ok") {
    return true;
  } else {
    return false;
  }
}

function isError(param) {
  if (/* XXX */param.tag === "Ok") {
    return false;
  } else {
    return true;
  }
}

function eqU(a, b, f) {
  if (/* XXX */a.tag === "Ok") {
    if (/* XXX */b.tag === "Ok") {
      return f(a.Arg0, b.Arg0);
    } else {
      return false;
    }
  } else if (/* XXX */b.tag === "Ok") {
    return false;
  } else {
    return true;
  }
}

function eq(a, b, f) {
  return eqU(a, b, Curry.__2(f));
}

function cmpU(a, b, f) {
  if (/* XXX */a.tag === "Ok") {
    if (/* XXX */b.tag === "Ok") {
      return f(a.Arg0, b.Arg0);
    } else {
      return 1;
    }
  } else if (/* XXX */b.tag === "Ok") {
    return -1;
  } else {
    return 0;
  }
}

function cmp(a, b, f) {
  return cmpU(a, b, Curry.__2(f));
}

exports.getExn = getExn;
exports.mapWithDefaultU = mapWithDefaultU;
exports.mapWithDefault = mapWithDefault;
exports.mapU = mapU;
exports.map = map;
exports.flatMapU = flatMapU;
exports.flatMap = flatMap;
exports.getWithDefault = getWithDefault;
exports.isOk = isOk;
exports.isError = isError;
exports.eqU = eqU;
exports.eq = eq;
exports.cmpU = cmpU;
exports.cmp = cmp;
/* No side effect */

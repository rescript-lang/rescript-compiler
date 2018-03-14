'use strict';

var Curry = require("./curry.js");

function getExn(param) {
  if (param) {
    return param[0];
  } else {
    throw new Error("getExn");
  }
}

function foldU(opt, $$default, f) {
  if (opt) {
    return f(opt[0]);
  } else {
    return $$default;
  }
}

function fold(opt, $$default, f) {
  return foldU(opt, $$default, Curry.__1(f));
}

function mapU(opt, f) {
  if (opt) {
    return /* Some */[f(opt[0])];
  } else {
    return /* None */0;
  }
}

function map(opt, f) {
  return mapU(opt, Curry.__1(f));
}

function flatMapU(opt, f) {
  if (opt) {
    return f(opt[0]);
  } else {
    return /* None */0;
  }
}

function flatMap(opt, f) {
  return flatMapU(opt, Curry.__1(f));
}

function getOrElse(opt, $$default) {
  if (opt) {
    return opt[0];
  } else {
    return $$default;
  }
}

function has(param) {
  if (param) {
    return /* true */1;
  } else {
    return /* false */0;
  }
}

function isEmpty(param) {
  if (param) {
    return /* false */0;
  } else {
    return /* true */1;
  }
}

exports.getExn = getExn;
exports.foldU = foldU;
exports.fold = fold;
exports.mapU = mapU;
exports.map = map;
exports.flatMapU = flatMapU;
exports.flatMap = flatMap;
exports.getOrElse = getOrElse;
exports.has = has;
exports.isEmpty = isEmpty;
/* No side effect */

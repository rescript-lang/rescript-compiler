'use strict';

var Curry = require("./curry.js");

function getExn(param) {
  if (param) {
    return param[0];
  } else {
    return /* assert false */0;
  }
}

function fold(opt, $$default, f) {
  if (opt) {
    return Curry._1(f, opt[0]);
  } else {
    return $$default;
  }
}

function map(opt, f) {
  if (opt) {
    return /* Some */[Curry._1(f, opt[0])];
  } else {
    return /* None */0;
  }
}

function flatMap(opt, f) {
  if (opt) {
    return Curry._1(f, opt[0]);
  } else {
    return /* None */0;
  }
}

function getOrElse(opt, $$default) {
  if (opt) {
    return opt[0];
  } else {
    return $$default;
  }
}

function exists(param) {
  if (param) {
    return /* true */1;
  } else {
    return /* false */0;
  }
}

function empty(param) {
  if (param) {
    return /* false */0;
  } else {
    return /* true */1;
  }
}

exports.getExn = getExn;
exports.fold = fold;
exports.map = map;
exports.flatMap = flatMap;
exports.getOrElse = getOrElse;
exports.exists = exists;
exports.empty = empty;
/* No side effect */

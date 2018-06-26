'use strict';

var Js_primitive = require("./js_primitive.js");

function some(x) {
  return Js_primitive.some(x);
}

function is_none(x) {
  return x === undefined;
}

function to_def(x) {
  if (x !== undefined) {
    return Js_primitive.valFromOption(x);
  } else {
    return undefined;
  }
}

function cons(x, y) {
  return /* :: */[
          x,
          y
        ];
}

function is_list_empty(x) {
  return x === /* [] */0;
}

var none = undefined;

exports.none = none;
exports.some = some;
exports.is_none = is_none;
exports.to_def = to_def;
exports.cons = cons;
exports.is_list_empty = is_list_empty;
/* No side effect */

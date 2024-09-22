'use strict';

let Primitive_option = require("./primitive_option.js");

function getItem(s, obj) {
  return Primitive_option.null_to_opt(obj.getItem(s));
}

function setItem(k, v, obj) {
  obj.setItem(k, v);
}

function removeItem(s, obj) {
  obj.removeItem(s);
}

function key(i, obj) {
  return Primitive_option.null_to_opt(obj.key(i));
}

exports.getItem = getItem;
exports.setItem = setItem;
exports.removeItem = removeItem;
exports.key = key;
/* No side effect */

'use strict';

let Curry = require("./curry.js");
let Caml_option = require("./caml_option.js");

function bind(x, f) {
  if (x == null) {
    return x;
  } else {
    return Curry._1(f, x);
  }
}

function iter(x, f) {
  if (!(x == null)) {
    return Curry._1(f, x);
  }
  
}

function fromOption(x) {
  if (x !== undefined) {
    return Caml_option.valFromOption(x);
  }
  
}

let from_opt = fromOption;

exports.bind = bind;
exports.iter = iter;
exports.fromOption = fromOption;
exports.from_opt = from_opt;
/* No side effect */

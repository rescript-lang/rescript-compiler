'use strict';

var Curry = require("./curry.js");
var CamlinternalLazy = require("./camlinternalLazy.js");

function from_fun(f) {
  return {
          LAZY_DONE: false,
          VAL: (function () {
              return Curry._1(f, undefined);
            })
        };
}

function from_val(v) {
  return {
          LAZY_DONE: true,
          VAL: v
        };
}

var Undefined = CamlinternalLazy.Undefined;

var force_val = CamlinternalLazy.force_val;

var is_val = CamlinternalLazy.is_val;

var lazy_from_fun = from_fun;

var lazy_from_val = from_val;

var lazy_is_val = CamlinternalLazy.is_val;

exports.Undefined = Undefined;
exports.force_val = force_val;
exports.from_fun = from_fun;
exports.from_val = from_val;
exports.is_val = is_val;
exports.lazy_from_fun = lazy_from_fun;
exports.lazy_from_val = lazy_from_val;
exports.lazy_is_val = lazy_is_val;
/* No side effect */

'use strict';

var Curry = require("../../lib/js/curry.js");

var v = /* record */[/* contents */0];

function f(x, x$1) {
  v[0] = v[0] + 1 | 0;
  return x$1 + x$1 | 0;
}

function $$return() {
  return v[0];
}

function Make(U) {
  var h = function (x, x$1) {
    console.log(f(x$1, x$1));
    return Curry._2(U[/* say */0], x$1, x$1);
  };
  return /* module */[/* h */h];
}

exports.v = v;
exports.f = f;
exports.$$return = $$return;
exports.Make = Make;
/* No side effect */

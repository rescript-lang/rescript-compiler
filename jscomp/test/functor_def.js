'use strict';

var Curry = require("../../lib/js/curry.js");

var v = [0];

function f(_, x) {
  v[0] = v[0] + 1 | 0;
  return x + x | 0;
}

function $$return() {
  return v[0];
}

function Make(U) {
  var h = function (_, x) {
    console.log(f(x, x));
    return Curry._2(U[/* say */0], x, x);
  };
  return /* module */[/* h */h];
}

exports.v = v;
exports.f = f;
exports.$$return = $$return;
exports.Make = Make;
/* No side effect */

'use strict';

var Curry = require("../../lib/js/curry");

function f(g) {
  return Curry.__1(g);
}

function map(f, lst) {
  if (lst) {
    return /* :: */[
            f(lst[0]),
            map(f, lst[1])
          ];
  }
  else {
    return /* [] */0;
  }
}

function map$1(f, lst) {
  return map(Curry.__1(f), lst);
}

exports.f   = f;
exports.map = map$1;
/* No side effect */

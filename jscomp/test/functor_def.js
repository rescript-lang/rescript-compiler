'use strict';

var Curry = require("../../lib/js/curry.js");
var Pervasives = require("../../lib/js/pervasives.js");

var v = /* record */{
  contents: 0
};

function f(x, x$1) {
  Pervasives.incr(v);
  return x$1 + x$1 | 0;
}

function $$return(param) {
  return v.contents;
}

function Make(U) {
  var h = function (x, x$1) {
    console.log(f(x$1, x$1));
    return Curry._2(U.say, x$1, x$1);
  };
  return {
          h: h
        };
}

exports.v = v;
exports.f = f;
exports.$$return = $$return;
exports.Make = Make;
/* No side effect */

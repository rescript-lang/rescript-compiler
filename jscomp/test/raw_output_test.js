'use strict';

var Curry = require("../../lib/js/curry.js");

function mk(fn) {
  return Curry._1(fn, void 0);
}

(Curry._1(function (){console.log('should works')}, void 0));

console.log((function () {
          return 1;
        })());

exports.mk = mk;
/*  Not a pure module */

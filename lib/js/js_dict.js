'use strict';

var Curry = require("./curry");

function map(f, source) {
  var target = { };
  Object.keys(source).forEach(function (key) {
        target[key] = Curry._1(f, source[key]);
        return /* () */0;
      });
  return target;
}

exports.map = map;
/* No side effect */

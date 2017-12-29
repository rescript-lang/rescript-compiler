'use strict';

var Export_keyword = require("./export_keyword.js");

function test2(v) {
  return {
          open: v.open,
          window: v.window
        };
}

function test(p) {
  return /* tuple */[
          p.catch,
          p.then
        ];
}

var $$case = Export_keyword.$$case;

var $$window = Export_keyword.$$window;

var $$switch = Export_keyword.$$switch;

exports.test2 = test2;
exports.test = test;
exports.$$case = $$case;
exports.$$window = $$window;
exports.$$switch = $$switch;
/* No side effect */

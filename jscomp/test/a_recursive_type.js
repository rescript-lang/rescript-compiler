'use strict';

var Curry = require("../../lib/js/curry.js");

function g(x) {
  return Curry._1(x._0, x);
}

var loop = g(/* A */{
      _0: g
    });

var x = /* A */{
  _0: g
};

var non_terminate = g(x);

exports.loop = loop;
exports.non_terminate = non_terminate;
/* loop Not a pure module */

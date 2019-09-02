'use strict';

var Curry = require("../../lib/js/curry.js");

function g(x) {
  return Curry._1(x.Arg0, x);
}

var loop = g(/* constructor */{
      tag: "A",
      Arg0: g
    });

var x = /* constructor */{
  tag: "A",
  Arg0: g
};

var non_terminate = g(x);

exports.loop = loop;
exports.non_terminate = non_terminate;
/* loop Not a pure module */

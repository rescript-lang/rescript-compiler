// GENERATED CODE BY BUCKLESCRIPT VERSION 0.6.0 , PLEASE EDIT WITH CARE
'use strict';

var Curry = require("../curry");

function g(x) {
  return Curry._1(x[0], x);
}

var loop = g(/* A */[g]);

var x = /* A */[g];

var non_terminate = g(x);

exports.loop          = loop;
exports.non_terminate = non_terminate;
/* loop Not a pure module */

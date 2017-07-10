'use strict';

var Curry        = require("../../lib/js/curry.js");
var Inner_define = require("./inner_define.js");

console.log(Curry._2(Inner_define.N[/* add */0], 1, 2));

/*  Not a pure module */

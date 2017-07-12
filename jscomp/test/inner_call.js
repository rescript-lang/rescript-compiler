'use strict';

var Curry        = require("../../lib/js/curry.js");
var Inner_define = require("./inner_define.js");

console.log(Curry._2(Inner_define.N[/* add */0], 1, 2));

function f(x) {
  return /* tuple */[
          Curry._1(Inner_define.N0[/* f1 */0], x),
          Curry._2(Inner_define.N0[/* f2 */1], x, x),
          Curry._3(Inner_define.N0[/* f3 */2], x, x, x),
          Curry._2(Inner_define.N1[/* f2 */0], x, x)
        ];
}

exports.f = f;
/*  Not a pure module */

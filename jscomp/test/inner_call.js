'use strict';

var Inner_define = require("./inner_define.js");

console.log(Inner_define.N[/* add */0](1, 2));

function f(x) {
  return /* tuple */[
          Inner_define.N0[/* f1 */0](x),
          Inner_define.N0[/* f2 */1](x, x),
          Inner_define.N0[/* f3 */2](x, x, x),
          Inner_define.N1[/* f2 */0](x, x)
        ];
}

exports.f = f;
/*  Not a pure module */

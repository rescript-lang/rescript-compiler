'use strict';

var Inner_define = require("./inner_define.js");

console.log(Inner_define.N.add(1, 2));

function f(x) {
  return /* tuple */[
          Inner_define.N0.f1(x),
          Inner_define.N0.f2(x, x),
          Inner_define.N0.f3(x, x, x),
          Inner_define.N1.f2(x, x)
        ];
}

exports.f = f;
/*  Not a pure module */

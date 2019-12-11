'use strict';


({ })["hi"] = "hello";

console.log("hi");

function f(x) {
  ({
      x: (console.log("hi"), x)
    }).x = x + 1 | 0;
  return /* () */0;
}

exports.f = f;
/*  Not a pure module */
